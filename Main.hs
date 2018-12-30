{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.List (intercalate)
import Prelude hiding (null, map, (+), (*), (-))
import Data.Monoid ((<>))
import qualified Prelude
import qualified GHC.Num
import Control.Monad.Fix
import Control.Monad.State

main = putStrLn "hello"

data Var (a :: k) where Var :: String -> Var a

instance Show (Var a) where show (Var s) = s

instance Num (Var Int) where
  fromInteger = Var . show
  negate (Var x) = Var $ "- " ++ x
  x + y = x + y
  x - y = x - y
  x * y = x * y
  abs (Var x) = Var $ "if " ++ x ++ " < 0 then -" ++ x ++ " else " ++ x
  signum (Var x) = Var $ "if " ++ x ++ " < 0 then -1 else 1"

data Null

data List a

class Explode a b where
  explode :: a -> b

-- instance Explode (a -> b) (c -> d) where
  -- explode = undefined

-- instance {-# OVERLAPPING #-} Explode (Var (a :-> b)) (Var a -> Var b) where
  -- explode = explode1

instance {-# OVERLAPPABLE #-} Explode (Var a) (Var a) where
  explode = id

instance {-# OVERLAPPABLE #-} Repack (Var a) (Var a) where
  repack = id

-- instance Explode c d => Explode (Var (a :-> b) -> c) ((Var a -> Var b) -> d) where
  -- explode x = \f -> explode $ (explode2 x) f

-- instance Explode b c => Explode (a -> b) (a -> c) where
  -- explode x = \f -> (explode (x f))

instance (Repack c a, Explode b d) => Explode (a -> b) (c -> d) where
  explode x =
    let x' = x . repack
    in \f -> explode (x' f)

-- instance (Explode b c, Explode a b) => Explode a c where
      -- f' = explode f
    -- in explode f'

instance Explode (Var b) c => Explode (Var (a :-> b)) (Var a -> c) where
  explode = \x -> explode . explode1 x

instance Explode (Var ((a :-> b)  :-> c)) ((Var a -> Var b) -> Var c) where
  explode = explode . explode
-- instance Explode (Var b) c => Explode (Var (a :-> b)) (Var a -> c) where
  -- explode = \x -> explode . explode1 x

class Repack a b where
  repack :: a -> b

instance Repack (Var a -> Var b) (Var (a :-> b)) where
  repack f =
    let Var res = f (Var "foo")
    in Var $ "(foo: " ++ res ++ ")"

foo :: Var Int -> Var Int
foo = explode (undefined :: Var (Int :-> Int))

bar :: Var Int -> Var (Int :-> Int)
bar = explode (undefined :: Var (Int :-> Int :-> Int))

baz :: Var Int -> Var Int -> Var Int
baz = explode (undefined :: Var (Int :-> Int :-> Int))

quux :: (Var Int -> Var Int) -> Var Int
quux = f2
  where
    f0 = undefined :: Var ((Int :-> Int) :-> Int)
    f1 = explode f0 :: Var (Int :-> Int) -> Var Int
    f2 = explode f1 :: (Var Int -> Var Int) -> Var Int

explode1 :: Var (a :-> b) -> Var a -> Var b
explode1 (Var f) (Var x) = Var $ "(" ++ f ++ " " ++ x ++ ")"

-- repack :: (Var a -> Var b) -> Var (a :-> b)

explode2 :: (Var (a :-> b) -> c) -> (Var a -> Var b) -> c
explode2 x = x . repack

    -- f1 = explode f0 :: Var (Int :-> Int) -> Var Int
    -- f2 = explode f1 :: (Var Int -> Var Int) -> Var Int

-- instance {-# OVERLAPPING #-} Explode (Var b) c => Explode (Var (a :-> b)) (Var a -> c) where
  -- explode = \x -> explode . explode1 x

-- instance {-# OVERLAPPING #-} Explode (Var (a :-> b)) (Var a -> Var b) where
  -- explode = explode1

-- instance (Explode a b, Explode c d) => Explode (a -> c) (b -> d) where
  -- explode = \x -> explode x

(==) :: Var a -> Var a -> Var Bool
(==) (Var l) (Var r) = Var $ l ++ " = " ++ r

const :: forall a b. Var a -> Var b -> Var a
const = explode (Var "(x: _: x)" :: Var (a :-> b :-> a))

ifThenElse :: forall a b. Var Bool -> Var a -> Var b -> Var (a :+: b)
ifThenElse = explode
    (Var "(x: y: z: if x then y else z)" :: Var (Bool :-> a :-> b :-> (a :+: b)))

someExpr :: Var Int
someExpr =
      lets
        (mdo
          x <- ew $ y + 1
          y <- ew 0
          pure x
        ) id

newtype Let a = Let (State [String] a)
  deriving (Functor, Applicative, Monad, MonadState [String], MonadFix)

lets :: Let (Var a) -> (Var a -> Var b) -> Var b
lets (Let a) f = case ts of { [] -> f res ; _ -> Var t }
  where
    t = intercalate "\n" $
      [ "let"
      ] <> ((<> ";") <$> ts) <>
      [ " in ", let Var foo = f res  in foo ]
    (res, ts) = runState a []

ew :: Var a -> Let (Var a)
ew (Var v) = do
  xs <- get
  let varName = "x" ++ show (length xs)
  put (xs ++ [varName ++ " = " ++ v])
  pure (Var varName)

isNull :: Var (a :-> Bool)
isNull = Var "builtins.isNull"

map :: Var ((a :-> b) :-> [a] :-> List [b])
map = Var "builtins.map"

(+) :: Var Int -> Var Int -> Var Int
(+) (Var l) (Var r) = Var $ l ++ " + " ++ r

(*) :: Var Int -> Var Int -> Var Int
(*) (Var l) (Var r) = Var $ l ++ " * " ++ r

(-) :: Var Int -> Var Int -> Var Int
(-) (Var l) (Var r) = Var $ l ++ " - " ++ r

someInts :: Var [Int]
someInts = Var "[1 2 3 4]"

null :: Var Null
null = Var "null"

stringAsChars :: Var ((Char :-> Char) :-> String :-> String)
stringAsChars = Var "pkgs.lib.stringAsChars"

-- stringAsChars' :: (Var Char -> Var Char) -> Var String -> Var String
-- stringAsChars' = explode stringAsChars

-- stringAsChars'' :: (Var Char -> Var Char) -> Var String -> Var String
-- stringAsChars'' = undefined

-- stringAsChars :: Var (Char :-> Char) -> Var String -> Var String
-- stringAsChars = undefined

fap, (.$) :: Var (a :-> b) -> Var a -> Var b
fap (Var f) (Var a) = Var ("(" ++ f ++ " " ++ a ++ ")")
(.$) = fap
infixr 1 .$

(.-) :: Var (b :-> c) -> Var (a :-> b) -> Var (a :-> c)
(.-) (Var f) (Var g) = Var $ "(x: " ++ f ++ "(" ++ g ++ " x" ++ ")" ++ ")"
infixr 8 .-

true :: Var Bool
true = Var "true"

false :: Var Bool
false = Var "false"

simple :: Var (String :+: Int)
simple = Var "if true then \"foo\" else 2"

readFile :: Var FilePath -> Var String
readFile v = undefined

data (:->) a b
infixr :->
data (:+:) a b
