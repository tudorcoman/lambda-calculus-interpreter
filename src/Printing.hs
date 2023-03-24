
module Printing (showExp) where

import Exp

showVar :: Var -> String
showVar v = getVar v


showExp :: ComplexExp -> String
showExp (CX v) = showVar v
showExp (CLam v e) = "\\" ++ showVar v ++ " -> " ++ showExp e
showExp (CApp e1 e2) = "{" ++ showExp e1 ++ " " ++ showExp e2 ++ "}"

