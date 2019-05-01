{-# LANGUAGE Safe #-}

module BohdanShcherbak (typecheck, eval) where

import AST
import DataTypes
import Data.Map hiding (map)

data Val  = VNum Integer | VBool Bool | VUnit | VPair Val Val | VList Val Val | VEmptyList
type Error p = (p, ErrKind)
data ErrKind = EUndefinedVariable Var | ETypeMismatch Type Type 
  | EIfMismatch Type Type | EFuncTypeMismatch FSym Type Type 
  | ECallOfFuncNotDefined FSym | EFstOfNotPair Type | ESndOfNotPair Type
  | EMatchMismatch Type Type | EWrongEmptyListType Type | EMatchNotList Type
type Env a = [(Var, a)]

instance Show ErrKind where
  show (EUndefinedVariable x) =
    "Undefined variable " ++ show x ++ "."
  show (ETypeMismatch t1 t2)  =
    "Type mismatch: expected " ++ show t1 ++ " but received " ++ show t2 ++ "."
  show (EIfMismatch t1 t2)    =
    "Type mismatch in the branches of an if: " ++ show t1 ++ " and " ++ show t2 ++ "."
  show (EFuncTypeMismatch fName t1 t2) =
    "Type mismatch in function " ++ show fName ++ " expected return type is " ++ show t1 ++ " and actual is " ++ show t2 ++ "."
  show (ECallOfFuncNotDefined fName) =
    "Undefined function " ++ show fName ++ "."
  show (EFstOfNotPair t) =
    "Type mismatch fst must take a pair received " ++ show t ++ " type."
  show (ESndOfNotPair t) =
    "Type mismatch snd must take a pair received " ++ show t ++ " type."
  show (EMatchMismatch t1 t2)    =
    "Type mismatch in the branches of match: " ++ show t1 ++ " and " ++ show t2 ++ "."
  show (EWrongEmptyListType t) =
    "Type mismatch empty list cannot have " ++ show t ++ " type."
  show (EMatchNotList t) =
    "Type mismatch match must take list as argument but " ++ show t ++ " is taken."
  

infixr 6 $>

($>) :: Maybe a -> Either a b -> Either a b
Just e  $> _ = Left e
Nothing $> e = e


checkEqual::(Type->Type->ErrKind)->p->Either (Error p) Type->Either (Error p) Type->Either (Error p) Type
checkEqual _ _ (Left err) _ = Left err
checkEqual _ _ _ (Left err) = Left err
checkEqual errkind p (Right t1) (Right t2) =
  if t1 == t2 then return t1
  else Left (p, errkind t1 t2)


inferType ::Map FSym (Type, Type) -> Env Type -> Expr p -> Either (Error p) Type
inferType _ γ (EVar p x) =
  case Prelude.lookup x γ of
    Just t  -> return t
    Nothing -> Left (p, EUndefinedVariable x)
inferType _ γ (ENum _ _)  = return TInt
inferType _ γ (EBool _ _) = return TBool
inferType fs γ (EUnary _ op e) =
  checkType fs γ e ta $>
  return tr
  where (ta, tr) = uopType op
inferType fs γ (EBinary _ op e1 e2) =
  checkType fs γ e1 et1 $>
  checkType fs γ e2 et2 $>
  return tr
  where (et1, et2, tr) = bopType op
inferType fs γ (ELet _ x ex eb) = do
  tx <- inferType fs γ ex
  inferType fs ((x, tx) : γ) eb
inferType fs γ (EIf p ec et ef) =
  checkType fs γ ec TBool $>
  checkEqual EIfMismatch p trBr flBr
  where
    trBr = inferType fs γ et
    flBr = inferType fs γ ef
inferType fs γ (EApp p fName argE) = 
  case fparams of 
    Nothing -> Left (p, ECallOfFuncNotDefined fName)
    Just (at, rt) ->
      checkType fs γ argE at $>
      return rt
  where 
    fparams = Data.Map.lookup fName fs::Maybe (Type, Type)
inferType _ _ (EUnit _) = return TUnit
inferType fs γ (EPair p el er) = do
  tl <- inferType fs γ el
  tr <- inferType fs γ er
  return (TPair tl tr)
inferType fs γ (EFst p e) = do
  pair <- inferType fs γ e
  case pair of
    TPair tl tr -> return tl
    t -> Left (p, EFstOfNotPair t)
inferType fs γ (ESnd p e) = do
  pair <- inferType fs γ e
  case pair of
    TPair tl tr -> return tr
    t -> Left (p, ESndOfNotPair t)
inferType _ _ (ENil p t) = 
  case t of
    TList t -> return $ TList t
    t -> Left (p, EWrongEmptyListType t)
inferType fs γ (ECons p x xs) = do
  t <- inferType fs γ x
  checkType fs γ xs (TList t) $> return (TList t)
inferType fs γ (EMatchL p e nilCase consCase) = do
  t <- inferType fs γ e
  case t of 
    TList t ->
      let 
        nct = inferType fs γ nilCase
        cct = inferType fs ((hvar,t):(tvar,TList t):γ) ce
        (hvar, tvar, ce) = consCase
      in checkEqual EMatchMismatch p nct cct
    t -> Left (p, EMatchNotList t)

checkType ::Map FSym (Type, Type) -> Env Type -> Expr p -> Type -> Maybe (Error p)
checkType fs γ e t =
  case inferType fs γ e of
    Left err -> Just err
    Right t' -> if t == t' then Nothing else Just (getData e, ETypeMismatch t' t)

uopType :: UnaryOperator -> (Type, Type)
uopType UNot = (TBool, TBool)
uopType UNeg = (TInt,  TInt)

bopType e = case e of
  BAnd -> tbool
  BOr  -> tbool
  BEq  -> tcomp
  BNeq -> tcomp
  BLt  -> tcomp
  BLe  -> tcomp
  BGt  -> tcomp
  BGe  -> tcomp
  BAdd -> tarit
  BSub -> tarit
  BMul -> tarit
  BDiv -> tarit
  BMod -> tarit
  where tbool = (TBool, TBool, TBool)
        tcomp = (TInt,  TInt,  TBool)
        tarit = (TInt,  TInt,  TInt)

typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck funcs vs e =
  case funcheck fs funcs of
    Just (p, err) -> Error p $ show err
    Nothing -> 
      case checkType fs γ e TInt of
        Nothing  -> Ok
        Just (p, err) -> Error p $ show err
  where 
    γ = map (\ x -> (x, TInt)) vs
    fs::Map FSym (Type, Type)
    fs = Data.Map.fromList $ Prelude.map (\f -> (funcName f, (funcArgType f, funcResType f))) funcs 

funcheck ::Map FSym (Type, Type) -> [FunctionDef p] -> Maybe (Error p)
funcheck _ []     = Nothing
funcheck fs (fun:funcs)  = 
  case checkType fs [(funcArg fun, funcArgType fun)] (funcBody fun) (funcResType fun) of 
    Just (p, ETypeMismatch t' t) -> Just (funcPos fun, EFuncTypeMismatch (funcName fun) t' t)
    Just err -> Just err
    Nothing -> funcheck fs funcs



ev :: Map FSym (FunctionDef p) -> Env Val -> Expr p -> Maybe Val
ev fs σ (EVar _ x)  = (Prelude.lookup) x σ
ev fs σ (ENum _ n)  = return $ VNum n
ev fs σ (EBool _ b) = return $ VBool b
ev _ _ (EUnit p) = return VUnit
ev fs σ (EUnary _ op e) = do
  v <- ev fs σ e
  evUOp op v
ev fs σ (EBinary _ op e1 e2) = do
  v1 <- ev fs σ e1
  v2 <- ev fs σ e2
  evBOp op v1 v2
ev fs σ (ELet _ x ex eb) = do
  v <- ev fs σ ex
  ev fs ((x, v) : σ) eb
ev fs σ (EIf _ ec et ef) = do
  VBool tf <- ev fs σ ec
  case tf of
    True -> ev fs σ et
    False -> ev fs σ ef
ev fs σ (EApp p fName argE) = do
  val <- ev fs σ argE
  ev fs ((funcArg fun, val):σ) (funcBody fun)
  where 
    Just fun = (Data.Map.lookup) fName fs
ev fs σ (EPair p el er) = do
  valL <- ev fs σ el
  valR <- ev fs σ er
  return (VPair valL valR)
ev fs σ (EFst p e) = do
  (VPair valL valR) <- ev fs σ e
  return valL
ev fs σ (ESnd p e) = do
  (VPair valL valR) <- ev fs σ e
  return valR
ev _ _ (ENil _ t) = return VEmptyList
ev fs σ (ECons p x xs) = do
  x <- ev fs σ x
  xs <- ev fs σ xs 
  return $ VList x xs
ev fs σ (EMatchL p e nilCase (hvar, tvar, expr)) = do
  list <- ev fs σ e
  case list of
    VEmptyList -> ev fs σ nilCase
    VList x xs -> ev fs ((hvar, x):(tvar, xs):σ) expr


evUOp UNot (VBool b) = Just . VBool $ not b
evUOp UNeg (VNum n)  = Just . VNum $ -n

evBOp BAnd (VBool b1) (VBool b2) = Just . VBool $ b1 && b2
evBOp BOr  (VBool b1) (VBool b2) = Just . VBool $ b1 || b2
evBOp BEq  (VNum n1)  (VNum n2)  = Just . VBool $ n1 == n2
evBOp BNeq (VNum n1)  (VNum n2)  = Just . VBool $ n1 /= n2
evBOp BLt  (VNum n1)  (VNum n2)  = Just . VBool $ n1 <  n2
evBOp BLe  (VNum n1)  (VNum n2)  = Just . VBool $ n1 <= n2
evBOp BGt  (VNum n1)  (VNum n2)  = Just . VBool $ n1 >  n2
evBOp BGe  (VNum n1)  (VNum n2)  = Just . VBool $ n1 >= n2
evBOp BAdd (VNum n1)  (VNum n2)  = Just . VNum  $ n1 + n2
evBOp BSub (VNum n1)  (VNum n2)  = Just . VNum  $ n1 - n2
evBOp BMul (VNum n1)  (VNum n2)  = Just . VNum  $ n1 * n2
evBOp BDiv (VNum n1)  (VNum n2)
  | n2 == 0   = Nothing
  | otherwise = Just . VNum  $ n1 `div` n2
evBOp BMod (VNum n1)  (VNum n2)
  | n2 == 0   = Nothing
  | otherwise = Just . VNum  $ n1 `mod` n2

eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval funcs args e =
  case ev fs σ e of
    Just (VNum n) -> Value n
    Nothing       -> RuntimeError
  where
    fs = Data.Map.fromList (Prelude.map (\f -> (funcName f, f)) funcs)
    σ  = map (\(x, n) -> (x, VNum n)) args
