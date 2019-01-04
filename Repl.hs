{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Data.IORef
import Data.Char (isControl)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Concurrent
import Data.Monoid
import Data.Void
import Control.Concurrent.Async
import Control.Monad
import qualified GHC.IO.Handle as H
import System.Process
import qualified Data.Aeson as Aeson
import Lib

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
            writeChan soutt res) `race`
          (forever $ do
            x <- readChan sint
            BS8.hPutStrLn sin x) `race`
          (act nixRepl)

        case res of
          Left (Left e) -> absurd e
          Left (Right e) -> absurd e
          Right x -> pure x

dropColors :: BS.ByteString -> BS.ByteString
dropColors t0 = do
  let
    (t1, rest1) = BS8.span (not . isControl) t0
    rest2 = BS8.dropWhile (\c -> c /= 'm') rest1
    t3 = case BS.stripPrefix "m" rest2 of
      Nothing -> error (show (t1, t0, rest1, rest2))
      Just t3 -> dropColors t3
  if BS.null rest1 then t1 else (t1 <> t3)

eval :: Aeson.FromJSON a => NixRepl -> Var a -> IO a
eval NixRepl{..} (Var v) = do
    let msg = "builtins.toJSON (" ++ v ++ ")"
    writeChan sint (BS8.pack msg)
    res <- readChan soutt
    _ <- readChan soutt -- empty line



    -- @"foo"@ -> @foo@
    let res'' = read (BS8.unpack res)

    case Aeson.decodeStrict res'' of
      Just x -> pure x
      Nothing -> error $
        "Cannot decode: " ++
        BS8.unpack res ++ "\n" ++
        BS8.unpack res''

evalVar :: Aeson.FromJSON a => Var a -> IO a
evalVar v = withNixRepl (\nixRepl -> eval nixRepl v)

bindVar :: NixRepl -> Var a -> IO (Var a)
bindVar NixRepl{..} (Var v) = do
      c <- readIORef count
      writeIORef count (c+1)
      let name = "x" ++ show c
      let msg = name ++ " = " ++ v
      writeChan sint (BS8.pack msg)
      _ <- readChan soutt -- empty line
      pure (Var name)

