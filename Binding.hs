module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show' l where
        show' (Macro m) = m
        show' other = show other

simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx stepFunc expr = case expandMacros expr of
    Left error -> Left error
    Right expandedExpr -> Right $ simplify stepFunc expandedExpr
  where
    expandMacros (Var z) = Right (Var z)
    expandMacros (App e1 e2) = App <$> expandMacros e1 <*> expandMacros e2
    expandMacros (Abs z e) = Abs z <$> expandMacros e
    expandMacros (Macro m) = case lookup m ctx of
      Just expr -> Right expr
      Nothing -> Left $ "" ++ m

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
