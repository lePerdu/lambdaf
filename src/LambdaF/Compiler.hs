{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaF.Compiler
  ( LambdaFError (..),
    lambdaFToStackIr,
    toCps,
    letrecReduce,
    normalizeLambdas,
    cpsReduce,
    lambdaShift,
    lambdaShiftUp,
    lambdaShiftDown,
    betaReduce,
    substitute,
    gatherFree,
    AbsState,
    unabstract,
    combToStackIr,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as M
import Data.Hashable
import qualified Data.Set as S
import qualified Data.Text as T
import LambdaF.StackIr.Language
import LambdaF.Language
import Lens.Micro.Platform
import Pinky.Brainfuck

identityC :: LambdaFCps
identityC = CLambda (CTerm 0)

toCps :: LambdaF -> LambdaFCps
-- x -> \c. c x
toCps (Term t) = CLambda (CAppl (CTerm 0) (CTerm (t + 1)))
-- x -> \c. c x
toCps (RecTerm t) = CLambda (CAppl (CTerm 0) (CRecTerm t))
-- k -> \c. c k
toCps (Constant k) = CLambda (CAppl (CTerm 0) (CConstant k))
-- cond x t f -> \c. C(x) (cond_c (C(f) c) (C(f) c))
toCps (Cond x t f) =
  CLambda
    ( CAppl
        (lambdaShiftUp (toCps x))
        ( CLambda
            ( CAppl
                ( CCond
                    (CAppl (shift (toCps t)) (CTerm 1))
                    (CAppl (shift (toCps f)) (CTerm 1))
                )
                (CTerm 0)
            )
        )
    )
  where
    shift = lambdaShift 2 0
-- e1 e2 -> \c. C(e2) (C(e1) id c)
toCps (Appl f v) =
  CLambda
    ( CAppl
        (lambdaShiftUp (toCps v))
        (CAppl (CAppl (lambdaShiftUp (toCps f)) identityC) (CTerm 0))
    )
-- \x. e -> \c. c (\c.\x. C(E) c)
toCps (Lambda e) =
  CLambda
    ( CAppl
        (CTerm 0)
        (CLambda (CLambda (CAppl (lambdaShift 2 1 (toCps e)) (CTerm 1))))
    )
-- letrec f = E -> \c. c (letrec f = C(E) id)
-- id is to not bind the inner continuation to f
toCps (Letrec e) =
  CLambda
    (CAppl (CTerm 0) (CLetrec (CAppl (lambdaShiftUp (toCps e)) identityC)))
toCps (Prim p) = CLambda (CAppl (CTerm 0) (makePrim argCount))
  where
    argCount = _primArgCount p

    -- In order for primitives to take multiple arguments as non-Cps, primitives
    -- have to be wrapped in a (very deep) sequence lambdas of the form:
    -- \c. c (\x1. \c. c (\x2. \c. c (\x3. \c. ... (op_c c x1 x2 x3 ...))))
    --
    -- This will make C(op x1 x2 x3 ...) beta-reduce to \c. op_c c x1 x2 x3 ...
    makePrim 1 = CLambda (CLambda (applArgs argCount (CPrim p (CTerm 1))))
    makePrim n = CLambda (CAppl (CTerm 0) (CLambda (makePrim (n - 1))))

    -- All of the arguments are applied to the operator at the end.
    --
    -- Argument indices skip 2 every time because of the extra continuation
    -- parameters. The result using DeBruijn indicies look like:
    -- op_c c (n-2) ... 4 2 0
    applArgs 0 expr = expr
    applArgs n expr = applArgs (n - 1) (CAppl expr (CTerm (2 * n - 2)))

-- | Shift term references in an expression.
--
-- lambdaShift level expr shifts all terms with index at least level. If expr is
-- a Lambda, it's local terms will not be shifted.
lambdaShift :: Int -> Int -> LambdaFCps -> LambdaFCps
lambdaShift shift level = go
  where
    go (CTerm x)
      | x >= level = CTerm (x + shift)
      | otherwise = CTerm x
    go t@(CRecTerm _) = t
    go k@(CConstant _) = k
    go (CCond t f) = CCond (go t) (go f)
    go (CAppl f v) = CAppl (go f) (go v)
    go (CLambda f) = CLambda (lambdaShift shift (level + 1) f)
    go (CLetrec f) = CLetrec (go f)
    go (CPrim p c) = CPrim p (go c)

lambdaShiftUp :: LambdaFCps -> LambdaFCps
lambdaShiftUp = lambdaShift 1 0

lambdaShiftDown :: LambdaFCps -> LambdaFCps
lambdaShiftDown = lambdaShift (-1) 1

-- -- | Shift letrec term references in an expression down.
-- lambdaShiftRec :: Int -> LambdaF -> LambdaFCps
-- lambdaShiftRec level = shift  where
--     shift t@(Term _) = t
--     shift (RecTerm x) | x >= level = RecTerm (x + 1)
--                       | otherwise  = RecTerm x
--     shift k@(Constant _) = k
--     shift (  Cond c t f) = Cond (shift c) (shift t) (shift f)
--     shift (  Appl f v  ) = Appl (shift f) (shift v)
--     shift (  Lambda f  ) = Lambda (shift f)
--     shift (  Letrec f  ) = Letrec (lambdaShiftRec (level + 1) f)
--     -- shift p@(Prim   _  ) = p

betaReduce :: LambdaFCps -> LambdaFCps
betaReduce (CCond t f) = CCond (betaReduce t) (betaReduce f)
betaReduce (CAppl f v) =
  let f' = betaReduce f
      v' = betaReduce v
   in case f' of
        CLambda e ->
          betaReduce $ lambdaShiftDown $ substitute 0 (lambdaShiftUp v') e
        _ -> CAppl f' v'
betaReduce (CLambda e) = CLambda (betaReduce e)
betaReduce (CLetrec e) = CLetrec (betaReduce e)
betaReduce (CPrim p c) = CPrim p (betaReduce c)
betaReduce e = e -- Constants and terms

-- | Substitute a Term (not RecTerm) index with an expression.
substitute :: Int -> LambdaFCps -> LambdaFCps -> LambdaFCps
substitute index repl = sub
  where
    sub (CTerm x) | x == index = repl
    sub (CCond t f) = CCond (sub t) (sub f)
    sub (CAppl f v) = CAppl (sub f) (sub v)
    sub (CLambda e) = CLambda (substitute (index + 1) (lambdaShiftUp repl) e)
    sub (CLetrec e) = CLetrec (sub e)
    sub (CPrim p c) = CPrim p (sub c)
    sub e = e -- Constant, RecTerm, and non-matching Term

letrecReduce :: LambdaFCps -> LambdaFCps
-- (letrec f = \c. e) id -> letrec g = e [g / f id]
letrecReduce orig@(CAppl (CLetrec (CLambda e)) (CLambda (CTerm 0))) =
  maybe
    orig
    CLetrec
    (trySub 0 eBeta)
  where
    -- Beta reduce the continuation
    eBeta = lambdaShiftDown $ substitute 0 identityC e

    -- Try to substitute all (f id) with g, but fail if f occurs without id
    trySub index = sub
      where
        sub (CAppl f@(CRecTerm x) (CLambda (CTerm 0))) | x == index = Just f
        sub (CRecTerm x) | x == index = Nothing
        sub (CCond t f) = CCond <$> sub t <*> sub f
        sub (CAppl f v) = CAppl <$> sub f <*> sub v
        sub (CLambda e) = CLambda <$> sub e
        sub (CLetrec e) = CLetrec <$> trySub (index + 1) e
        sub (CPrim p c) = CPrim p <$> sub c
        sub e = Just e
letrecReduce (CCond t f) = CCond (letrecReduce t) (letrecReduce f)
letrecReduce (CAppl f v) = CAppl (letrecReduce f) (letrecReduce v)
letrecReduce (CLambda e) = CLambda (letrecReduce e)
letrecReduce (CLetrec e) = CLetrec (letrecReduce e)
letrecReduce (CPrim p c) = CPrim p (letrecReduce c)
letrecReduce e = e -- Constants and terms

gatherFree :: Int -> LambdaFCps -> S.Set Int
gatherFree depth = gather
  where
    gather (CTerm x) | x >= depth = S.singleton x
    gather (CAppl f v) = gather f <> gather v
    gather (CCond t f) = gather t <> gather f
    gather (CLambda e) = S.map (subtract 1) $ gatherFree (depth + 1) e
    gather (CLetrec e) = gather e
    gather (CPrim _ c) = gather c
    gather _ = S.empty

normalizeLambdas :: LambdaFCps -> LambdaFCps
normalizeLambdas (CLambda e) = applFrees
  where
    getLambdaDepth acc (CLambda expr) = getLambdaDepth (acc + 1) expr
    getLambdaDepth acc expr = (acc, expr)

    (lambdaDepth, baseExpr) = getLambdaDepth 1 e

    -- Recursively normalize
    normBase = normalizeLambdas baseExpr

    -- Free variables have to be substituted in ascending order so that later
    -- free variable substitutions do not conflict with them (i.e. if 2 is
    -- replaced by 1 and then 1 is replaced by 3, 2 will have also been
    -- replaced by 3).
    freeVars = S.toAscList $ gatherFree lambdaDepth normBase
    freeCount = length freeVars
    newExpr =
      foldl' (\e (idx, new) -> substitute idx (CTerm new) e) normBase $
        zip freeVars [lambdaDepth ..]

    rewrapped = iterate CLambda newExpr !! (lambdaDepth + freeCount)

    applFrees =
      foldr
        (\idx expr -> CAppl expr (CTerm (idx - lambdaDepth)))
        rewrapped
        freeVars
normalizeLambdas (CAppl f v) = CAppl (normalizeLambdas f) (normalizeLambdas v)
normalizeLambdas (CCond t f) = CCond (normalizeLambdas t) (normalizeLambdas f)
normalizeLambdas (CLetrec e) = CLetrec (normalizeLambdas e)
normalizeLambdas (CPrim p c) = CPrim p (normalizeLambdas c)
normalizeLambdas e = e

cpsReduce :: LambdaFCps -> LambdaFCps
-- TODO Can the middle betaReduce be removed since we are normalizing anyway?
cpsReduce = normalizeLambdas . betaReduce . letrecReduce . betaReduce

-- | Types of errors during compilation
--
-- This includes lambda abstraction and StackIr generation.
--
-- TODO Should this exist, or should the parsing step be assumed correct?
data LambdaFError
  = TermOutOfBounds Int Int
  | RecTermOutOfBounds Int Int
  deriving (Show)

-- | State for lambda abstraction algorithm
--
-- Contains the number of arguments of the current function and the number of
-- extra values pushed on the stack on top of the arguments.
type AbsState = (Int, Int)

-- | Delete 'n' items after the 'm'th item
deltComb :: Int -> Int -> LambdaComb -> LambdaComb
deltComb _ 0 = id
deltComb m n = LDel (m + 1) . deltComb m (n - 1)

-- | Extract the 'i'th argument and delete all other arguments
exedComb :: Int -> Int -> Int -> LambdaComb
exedComb m n i =
  deltComb m (n - i - 1) $ LDup (m + 1) $ deltComb (m + 1) (i + 1) $ LId

unabstract :: AbsState -> LambdaFCps -> Either LambdaFError LambdaComb
-- No free variables, so delete the arguments
unabstract (n, p) expr
  | n > 0 && S.null (gatherFree 0 expr) =
    deltComb p n <$> unabstract (0, p) expr
-- Identity function
unabstract (n, p) (CLambda (CTerm 0)) = Right LId
-- Application on an argument
unabstract (n, p) (CAppl f (CTerm x))
  | x < n = LDup (p + n - x) <$> unabstract (n, p + 1) f
  | otherwise = Left $ TermOutOfBounds n x
-- Application on a constant
unabstract (n, p) (CAppl f (CConstant k)) =
  LPush (LConst k) <$> unabstract (n, p + 1) f
-- Application of a user-defined function
-- TODO Implement this once user-defined functions are "fist-class"
-- unabstract (n, p) (CAppl (CConstant f@(LFFuncRef _)) v) =
--     LPush (unabstract (n, 0) v) (LConst f)
-- Conditional
unabstract (n, p) (CCond t f) =
  let unab = unabstract (n, p - 1)
   in LCond <$> unab t <*> unab f
-- Just an argument, so the remaining arguments and temporary variables can be
-- deleted
unabstract (n, p) (CTerm x)
  | x < n = Right $ exedComb p n x
  | otherwise = Left $ TermOutOfBounds n x
-- Just a letrece reference
unabstract _ (CRecTerm x) = Right $ LRecTerm x
-- Just a constant
unabstract _ (CConstant k) = Right $ LConst k
-- General function application, where both the function and the argument may
-- be arbitrarily complex
-- The argument is made into a closure and then passed to the function
unabstract (n, p) (CAppl f v) = do
  val <- unabstract (n, 0) v
  cont <- unabstract (n, p + 1) f
  Right $ LPush val (LMkcl p n cont)
-- Lambda abstraction
--
-- Lambdas are normalized so that there are never any temporaries left, so
-- it can be set to 0
-- TODO Validate this instead of assuming it. Better yet, reflect that invariant
-- in the data type.
unabstract (n, p) (CLambda e) = unabstract (n + 1, 0) e
unabstract r (CLetrec e) = LLetrec <$> unabstract r e
unabstract (n, p) (CPrim prim c) =
  LPrim prim <$> (unabstract (n, p - _primArgCount prim + 1) c)

-- | Internal state for keeping track of functions during compilation
--
-- Generated functions (i.e. not user-defined ones) are named and stored based
-- on the hash of their contents, so identical functions will only be created
-- once.
type FunctionsMap = M.HashMap T.Text [StackIrInstr]

-- | Internal state for managing letrec identifiers
type LetRecStack = [T.Text]

-- | Reader state for compilation
data CompilerContext = CompilerContext {_letrecStack :: LetRecStack}

makeLenses ''CompilerContext

-- | Monad stack for managing state during compilation
--
-- TODO Add ExceptT
type CompilerM =
  ReaderT CompilerContext (StateT FunctionsMap (Except LambdaFError))

-- | Generate a StackIr for a LambdaComb program
combToStackIr :: LambdaComb -> Either LambdaFError StackIr
combToStackIr comb =
  toStackIr
    <$> runExcept (execStateT (runReaderT genStackIr initContext) M.empty)
  where
    initContext = CompilerContext []

    toStackIr = StackIr . map (uncurry StackIrFunc) . M.toList

    genStackIr = registerFunction "main" comb

    genIdent comb = T.pack ("__" ++ show (abs $ hash comb))

    getLetrecRef :: Int -> CompilerM T.Text
    getLetrecRef index = do
      letrecs <- asks _letrecStack
      let len = length letrecs
      if index < len
        then pure $ letrecs !! index
        else throwError $ RecTermOutOfBounds len index

    registerFunction ident comb = do
      -- Use an existing one if already defined (assuming same if the hashes
      -- match)
      existing <- M.member ident <$> get
      unless existing $ do
        newFunc <- genFunc comb
        modify $ M.insert ident newFunc

    genStackValue (LConst x) = pure $ StackLiteral (fromIntegral x)
    genStackValue (LRecTerm t) = StackIdent <$> getLetrecRef t
    genStackValue x = do
      -- Use the code's hash to generate a name for it
      let ident = genIdent x
      registerFunction ident x
      pure $ StackIdent ident

    -- Generates the code for a function (line of combinators)
    genFunc LId = pure []
    -- TODO LConst should never actually happen, right? since a literal can be
    -- used in-place, not in a function
    genFunc (LConst x) = pure [StackPush (StackLiteral (fromIntegral x))]
    -- Create a new identifier for this function and save it so LRecTerm values
    -- can be resolved later
    genFunc (LLetrec c) = do
      let ident = genIdent c
      -- Push the identifier to the letrec stack while compiling the function
      -- so that LRecTerm references will be correct
      local (letrecStack %~ (ident :)) $
        registerFunction ident c
      pure [StackPush (StackIdent ident)]
    genFunc (LRecTerm t) = do
      ident <- getLetrecRef t
      pure [StackPush (StackIdent ident)]
    genFunc (LCond t f) = do
      t' <- genStackValue t
      f' <- genStackValue f
      pure [StackCond t' f']
    genFunc (LPush v c) = do
      v' <- genStackValue v
      fmap (StackPush v' :) (genFunc c)
    genFunc (LDup i c) = fmap (StackDup i :) (genFunc c)
    genFunc (LDel i c) = fmap (StackDel i :) (genFunc c)
    genFunc (LMkcl p n c)
      | n == 0 = genFunc c -- No arguments, no reason to make a closure
      | otherwise = fmap (stackCode ++) (genFunc c)
      where
        stackCode =
          -- Copy all arguments, skipping over the function and temporaries
          replicate n (StackDup (p + n + 1))
            ++ [
                 -- Copy function
                 StackDup (n + 1),
                 -- Pack closure
                 StackPack (n + 1),
                 -- Delete old function
                 StackDel 2
               ]
    genFunc (LPrim p c) = do
      fmap (_primCode p ++) (genFunc c)

lambdaFToStackIr :: LambdaF -> Either LambdaFError StackIr
lambdaFToStackIr lambdaF = do
  let cps = toCps lambdaF
  let normalized = cpsReduce (CAppl cps identityC)
  comb <- unabstract (0, 0) normalized
  combToStackIr comb
