{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Aeson as Aeson
import Data.Char (isControl)
import Data.Monoid
import Data.Void
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Timeout
import System.Process
import qualified GHC.IO.Handle as H

data Var a = Var String

instance (Baz c, Foo d) => Foo (c -> d) where
  foo str = \mkF -> do
    let
      res = baz mkF
    foo $ "(" ++ "(" ++ str ++ ") (" ++ res ++ "))"

instance Show (Var a) where
  show (Var str) = str

class Foo b where
  foo :: String -> b

instance Foo (Var a) where
  foo = Var

class Baz a where
  baz :: a -> String

instance Baz (Var b) where
  baz (Var str) = str

instance Baz c => Baz (Var a -> c) where
  baz = \mkF ->
    let
      v = baz (mkF (Var "foo"))
    in ("(foo: " ++ v ++ ")")

f0 :: Var Int
f0 = Var "x"

f1 :: Var Int -> Var Int
f1 _ = f0

f2 :: Var Int -> Var Int -> Var Int
f2 _ _ = Var "x"

f3 :: Var Int -> Var Int -> Var Int -> Var Int
f3 _ _ _ = Var "x"

data NixRepl = NixRepl
  { sint :: Chan BS.ByteString
  , soutt :: Chan BS.ByteString
  , serrt :: Chan BS.ByteString
  , count :: IORef Int
  }

withNixRepl :: (NixRepl -> IO a) -> IO a
withNixRepl act = do
    nixRepl@NixRepl{..} <-
      NixRepl <$> newChan <*> newChan <*> newChan <*> newIORef 0

    let cp = (shell "nix repl")
              { std_in = CreatePipe
              , std_out = CreatePipe
              , std_err = CreatePipe
              }

    withCreateProcess cp $
      \(Just sin) (Just sout) (Just serr) _ -> do
        H.hSetBuffering sin H.LineBuffering
        H.hSetBuffering sout H.LineBuffering
        H.hSetBuffering serr H.LineBuffering

        -- "Welcome to Nix ..."
        _ <- BS8.hGetLine sout
        _ <- BS8.hGetLine sout

        res <-
          (forever $ do
            res <- dropColors <$> BS8.hGetLine sout
            -- putStrLn $ "READ: " ++ BS8.unpack res
            writeChan soutt res) `race`
          (forever $ do
            x <- readChan sint
            -- putStrLn $ "WRITE: " ++ BS8.unpack x
            BS8.hPutStrLn sin x) `race`
          (act nixRepl)

        case res of
          Left (Left e) -> absurd e
          Left (Right e) -> absurd e
          Right x -> pure x

toNix :: Aeson.ToJSON a => a -> Var a
toNix x = Var $ "builtins.fromJSON (" ++ show (BL8.unpack (Aeson.encode x)) ++ ")"

eval :: Aeson.FromJSON a => Var a -> NixRepl -> IO a
eval (Var v) NixRepl{..} = do
    let msg = "builtins.toJSON (" ++ v ++ ")"
    writeChan sint (BS8.pack msg)
    res <- readChan soutt
    _ <- readChan soutt -- empty line
    let Just ('"', res') = BS8.uncons res
        Just (res'', '"') = BS8.unsnoc res'
    case Aeson.decodeStrict res'' of
      Just x -> pure x
      Nothing -> error $ "Cannot decode: " ++ BS8.unpack res

var :: Var a -> NixRepl -> IO (Var a)
var (Var v) NixRepl{..} = do
      c <- readIORef count
      writeIORef count (c+1)
      let name = "x" ++ show c
      let msg = name ++ " = " ++ v
      writeChan sint (BS8.pack msg)
      _ <- readChan soutt -- empty line
      pure (Var name)

test :: IO ()
test = do
    x <- withNixRepl (\nixRepl -> do
        let
          xs = toNix ([1 .. 10 ]) :: Var [Int]
          map' = foo "builtins.map" :: (Var a -> Var b) -> Var [a] -> Var [b]
          plus = foo "(x: y: x + y)" :: Var Int -> Var Int -> Var Int
        y <- var (Var "3 + 4") nixRepl :: IO (Var Int)

        eval (map' (plus y) xs) nixRepl
        )
    print x

dropColors :: BS.ByteString -> BS.ByteString
dropColors t0 = do
  let
    (t1, rest1) = BS8.span (not . isControl) t0
    rest2 = BS8.dropWhile (\c -> c /= 'm') rest1
    t3 = case BS.stripPrefix "m" rest2 of
      Nothing -> error (show (t1, t0, rest1, rest2))
      Just t3 -> dropColors t3
  if BS.null rest1 then t1 else (t1 <> t3)
