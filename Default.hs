module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vu = Var "u"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App (Abs "x" $ App vf (App vx vx)) (Abs "x" $ App vf (App vx vx))

-- 4.1. Boolean encodings
bTrue = Abs "x" $ Abs "y" $ vx
bFalse = Abs "x" $ Abs "y" $ vy
bAnd = Abs "p" $ Abs "q" $ App (App (Var "p") (Var "q")) (Var "p")
bOr = Abs "p" $ Abs "q" $ App (App (Var "p") (Var "p")) (Var "q")
bNot = Abs "p" $ App (App (Var "p") bFalse) bTrue
bXor = Abs "p" $ Abs "q" $ App (App (Var "p") (App bNot (Var "q"))) (Var "q")

-- 4.2. Pair encodings
pair = Abs "x" $ Abs "y" $ Abs "f" $ App (App vf vx) vy
first = Abs "p" $ App vp bTrue where vp = Var "p"
second = Abs "p" $ App vp bFalse where vp = Var "p"

-- 4.3. Natural number encodings
n0 = Abs "f" $ Abs "x" $ vx
n1 = Abs "f" $ Abs "x" $ App vf vx
n2 = Abs "f" $ Abs "x" $ App vf (App vf vx)
nSucc = Abs "n" $ Abs "f" $ Abs "x" $ App vf (App (App vn vf) vx) where vn = Var "n"; vf = Var "f"; vx = Var "x"

nPred = Abs "n" $ Abs "f" $ Abs "x" $
  App (App (App (Var "n") (Abs "g" $ Abs "h" $ App (Var "h") (App (Var "g") (Var "f")))) (Abs "u" (Var "x"))) (Abs "u" (Var "u"))
nAdd = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" $ App (App vm vf) (App (App vn vf) vx) where vm = Var "m"; vn = Var "n"; vf = Var "f"; vx = Var "x"
nSub = Abs "m" $ Abs "n" $ App (App vn nPred) vm where vm = Var "m"; vn = Var "n"
nMult = Abs "m" $ Abs "n" $ Abs "f" $ App vm (App vn vf) where vm = Var "m"; vn = Var "n"; vf = Var "f"

-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    , ("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
