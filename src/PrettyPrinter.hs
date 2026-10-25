module PrettyPrinter
  ( printTerm  ,     -- pretty printer para terminos
    printType        -- pretty printer para tipos
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i || isLet c || isCons c || isRL c) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c || isLet c || isCons c || isRL c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c

pp ii vs (Let t1 t2) =
  text "let "
    <> text (vs !! ii)
    <> text " = "
    <> pp ii vs t1
    <> text " in "
    <> pp (ii + 1) vs t2

pp _ _ Zero      = text "0"

pp ii vs (Suc t) = 
  text "suc " 
    <> parensIf (isLam t || isApp t || isR t) (pp ii vs t)

pp ii vs (Rec t1 t2 t3) = sep 
  [ text "R"
  , nest 1 (parensIf (isLam t1 || isApp t1 || isSuc t1 || isR t1) (pp ii vs t1))
  , nest 1 (parensIf (isLam t2 || isApp t2 || isSuc t2 || isR t2) (pp ii vs t2))
  , nest 1 (parensIf (isLam t3 || isApp t3 || isSuc t3 || isR t3) (pp ii vs t3))
  ]

pp _ _ (Nil) = text "nil"
pp ii vs (Cons h t) = sep
  [ text "cons"
  , nest 1 (parensIf (isLam h || isApp h || isCons h || isRL h) (pp ii vs h))
  , nest 1 (parensIf (isLam t || isRL t) (pp ii vs t))
  ]
pp ii vs (RL a b c) = sep
  [ text "RL"
  , nest 1 (parensIf (isLam a || isApp a || isCons a || isRL a) (pp ii vs a))
  , nest 1 (parensIf (isLam b || isApp b || isCons b || isRL b) (pp ii vs b))
  , nest 1 (parensIf (isLam c) (pp ii vs c))
  ]

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

isSuc :: Term -> Bool
isSuc (Suc _) = True
isSuc _       = False

isR :: Term -> Bool
isR (Rec _ _ _) = True
isR _           = False

isCons :: Term -> Bool
isCons (Cons _ _) = True
isCons _          = False

isRL :: Term -> Bool
isRL (RL _ _ _) = True
isRL _          = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType NatT   = text "Nat"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]

isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let t1 t2       ) = fv t1 ++ fv t2
fv Zero           = []
fv (Suc t)        = fv t
fv (Rec t1 t2 t3) = fv t1 ++ fv t2 ++ fv t3
fv Nil = []
fv (Cons h t) = fv h ++ fv t
fv (RecL a b c) = fv a ++ fv b ++ fv c

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

