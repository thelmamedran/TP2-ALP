module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion_aux []

indice :: Eq a => [a] -> a -> (Bool, Int)
indice = indice_aux 0
      where indice_aux i [] x = (False, -1)
            indice_aux i (x:xs) y = if (x == y) then (True, i) else indice_aux (i+1) xs y

conversion_aux :: [String] -> LamTerm -> Term
conversion_aux l (LVar x) = let (bool, i) = indice l x
                              in if bool then (Bound i) else (Free (Global x))
conversion_aux l (LAbs x t term) = (Lam t (conversion_aux (x:l) term))
conversion_aux l (LApp t1 t2) = ((conversion_aux l t1) :@: (conversion_aux l t2))
-- a chequear::::::::
conversion_aux l (LLet x t1 t2) = Let (conversion_aux l t1) (conversion_aux (x:l) t2)

conversion_aux l LZero           = Zero
conversion_aux l (LSuc t)        = Suc (conversion_aux l t)
conversion_aux l (LRec t1 t2 t3) = Rec (conversion_aux l t1) (conversion_aux l t2) (conversion_aux l t3)

conversion_aux l LNil = Nil
conversion_aux l (LCons t1 t2) = Cons (conversion_aux l t1) (conversion_aux l t2)

----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
-- sub i t (Let u v) = -- COMPLETAR
sub _ _ Zero           = Zero
sub i t (Suc u)        = Suc (sub i t u)
sub i t (Rec t1 t2 t3) = Rec (sub i t t1) (sub i t t2) (sub i t t3)
sub _ _ Nil            = Nil
sub i t (Cons u v)     = Cons (sub i t u) (sub i t v)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f
quote (VNum NZero)    = Zero
quote (VNum (NSuc n)) = Suc (quote (VNum n))
quote (VList VNil)         = Nil
quote (VList (VCons n l))  = Cons (quote (VNum n)) (quote (VList l))

-- evalúa un término en un entorno dado
eval :: NameEnv Value Type -> Term -> Value 
eval nvs (Lam t body) = VLam t body
eval nvs (t1 :@: t2)  = case eval nvs t1 of
                          VLam t body -> let v2    = eval nvs t2
                                             body' = sub 0 (quote v2) body
                                         in eval nvs body'
                          _           -> error "Se intentó aplicar un término que no es una función."
eval nvs (Free n)     = case lookup n nvs of 
                          Just (v, _) -> v
                          Nothing     -> error "Variable global no definida."
                          
eval nvs (Let t1 t2) = let t1' = eval nvs t1 in
                          eval nvs (sub 0 (quote t1') t2)

eval _ Zero      = VNum NZero
eval nvs (Suc t) = case eval nvs t of
                     VNum nv -> VNum (NSuc nv)
                     _ -> error "Se esperaba un número en Suc."
eval nvs (Rec t1 t2 t3) = case eval nvs t3 of
                            VNum NZero     -> eval nvs t1
                            VNum (NSuc nv) -> let t3' = quote (VNum nv)
                                              in eval nvs ((t2 :@: (Rec t1 t2 t3')) :@:  t3')
                            _ -> error "Se esperaba un número en el tercer argumento de R."

eval nvs Nil = VList VNil
eval nvs (Cons h t) = case eval nvs h of
                        VNum x -> case eval nvs t of
                                  VList xs -> VList (VCons x xs)
                                  _ -> error "Lista mal definida."
                        _ -> error "Un elemento de la lista no es un natural."

-- BORRAR?
eval nvs (RecL t1 t2 l) = case eval nvs l of
                            VList xs -> case xs of
                                            VNil -> eval nvs t1
                                            VCons x xs -> eval nvs ((t2 :@: (quote (VNum x))) :@: (RecL t1 t2 (quote (VList xs))))
                            _ -> error "Se intentó aplicar RL en un término que no es una lista."
                            

----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t1 t2) = infer' c e t1 >>= \tt -> infer' (tt : c) e t2

infer' _ _ Zero    = ret NatT

infer' c e (Suc t) = 
  infer' c e t >>= \tt -> 
    if   tt == NatT
    then ret NatT
    else matchError NatT tt

infer' c e (Rec t1 t2 t3) = 
  infer' c e t1 >>= \t1t -> 
  infer' c e t2 >>= \t2t ->
  infer' c e t3 >>= \t3t ->
    if   t3t /= NatT
    then matchError NatT t3t
    else if   t2t == FunT t1t (FunT NatT t1t)
         then ret t1t
         else matchError (FunT t1t (FunT NatT t1t)) t2t

infer' c e Nil = ret ListT
infer' c e (Cons x xs) = infer' c e x >>= \tx -> infer' c e xs >>= \txs -> 
    case tx of
      NatT -> case txs of
              ListT -> ret ListT
              _ -> matchError ListT txs
      _ -> matchError NatT tx

-- BORRAR?
infer' c e (RecL t1 t2 t3) = 
      infer' c e t1 >>= \tt1 -> 
      infer' c e t2 >>= \tt2 -> 
      infer' c e t3 >>= \tt3 ->
    case tt2 of 
      FunT NatT t2' -> case t2' of 
                          FunT t2'' t3' -> if (t2'' == tt1 && t3' == tt1) then case tt3 of
                                                                                ListT -> ret tt1
                                                                                _ -> matchError ListT tt3
                                                    else matchError (FunT NatT (FunT tt1 tt1)) tt2
                          _ -> matchError (FunT NatT (FunT tt1 tt1)) tt2
      _ -> matchError (FunT NatT (FunT tt1 tt1)) tt2
