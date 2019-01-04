{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Monad.Fix
import Data.Monoid
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad.State
import Data.List (intercalate)

data Var a = Var String

instance Show (Var a) where
  show (Var str) = str

instance (GenNixNegative c, GenNix d) => GenNix (c -> d) where
  genNix str = \mkF -> do
    let
      res = genNixNegative mkF
    genNix $ "(" ++ "(" ++ str ++ ") (" ++ res ++ "))"

class GenNix b where
  genNix :: String -> b

instance GenNix (Var a) where
  genNix = Var

class GenNixNegative a where
  genNixNegative :: a -> String

instance GenNixNegative (Var b) where
  genNixNegative (Var str) = str

instance GenNixNegative c => GenNixNegative (Var a -> c) where
  genNixNegative = \mkF ->
    let
      -- a dummy variable name
      varName = "x"
      v = genNixNegative (mkF (Var varName))
    in ("(" ++ varName ++ ": " ++ v ++ ")")

toNix :: Aeson.ToJSON a => a -> Var a
toNix x = Var $ "builtins.fromJSON (" ++ show (BL8.unpack (Aeson.encode x)) ++ ")"

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
