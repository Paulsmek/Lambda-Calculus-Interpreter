module Lambda where

import Data.List (nub, (\\))
import Data.List (sort)

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (App e1 e2) = nub $ vars e1 ++ vars e2
vars (Abs x e) = nub (x : vars e)
vars (Macro _) = []

-- 1.2.
freeVars :: Lambda -> [String]
freeVars expr = nub $ go expr []
  where
    go (Var x) bound = if x `elem` bound then [] else [x]
    go (App e1 e2) bound = go e1 bound ++ go e2 bound
    go (Abs x e) bound = go e (x : bound)
    go (Macro _) _ = []


-- 1.3.
newVar :: [String] -> String
newVar xs = 
    let
        singleLetters = map (:[]) ['a'..'z']
        doubleLetters = [ a:b:[] | a <- ['a'..'z'], b <- ['a'..'z'] ]
        tripleLetters = [ a:b:c:[] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z'] ]
        allNames = singleLetters ++ doubleLetters ++ tripleLetters
    in head $ filter (`notElem` xs) allNames


-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Abs _ e) = isNormalForm e
isNormalForm (Macro _) = True


-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = go e1
  where
    go (Var y) = if x == y then e2 else Var y
    go (App e1' e2') = App (go e1') (go e2')
    go (Abs y e')
      | y == x = Abs y e'
      | y `elem` freeVars e2 = let newY = newVar (vars e1 ++ vars e2)
                              in Abs newY (go $ subst y (Var newY) e')
      | otherwise = Abs y (go e')
    go (Macro y) = Macro y
    subst y newE (Var z) = if y == z then newE else Var z
    subst y newE (App e1' e2') = App (subst y newE e1') (subst y newE e2')
    subst y newE (Abs z e') = Abs z (subst y newE e')
    subst _ _ (Macro z) = Macro z 

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2)
  | not (isNormalForm e1) = App (normalStep e1) e2
  | otherwise = App e1 (normalStep e2)
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e


-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x e1) e2)
  | isNormalForm e2 = reduce x e1 e2
  | otherwise = App (Abs x e1) (applicativeStep e2)
applicativeStep (App e1 e2)
  | not (isNormalForm e1) = App (applicativeStep e1) e2
  | otherwise = App e1 (applicativeStep e2)
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep e = e


-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify stepFunc expr = go expr
  where
    go e | isNormalForm e = [e]
         | otherwise = e : go (stepFunc e)


normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
