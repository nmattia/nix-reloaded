{-# LANGUAGE FlexibleInstances #-}

module Pkgs where

import qualified Data.Map as Map
import Lib

instance Num (Var Int) where
  fromInteger = Var . show
  negate (Var x) = Var $ "- " ++ x
  x + y = x `plus_int` y
  x - y = x `minus_int` y
  x * y = x `times_int` y
  abs (Var x) = Var $ "if " ++ x ++ " < 0 then -" ++ x ++ " else " ++ x
  signum (Var x) = Var $ "if " ++ x ++ " < 0 then -1 else 1"

infx :: String -> Var a -> Var b -> Var c
infx op (Var l) (Var r) = Var $ l ++ " " ++ op ++ " " ++ r

plus_int, minus_int, times_int  :: Var Int -> Var Int -> Var Int
plus_int = infx "+"
minus_int = infx "-"
times_int = infx "*"

plus_double, minus_double, times_double :: Var Double -> Var Double -> Var Double
plus_double = infx "+"
minus_double = infx "-"
times_double = infx "*"

map :: (Var a -> Var b) -> Var [a] -> Var [b]
map = genNix "builtins.map"

(//) :: Var (Map.Map k v) -> Var (Map.Map k v) -> Var (Map.Map k v)
(//) = infx "//"
