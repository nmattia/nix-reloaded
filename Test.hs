{-# LANGUAGE RecursiveDo #-}

import Data.Monoid
import Lib
import Test.Tasty.HUnit
import Repl
import qualified Pkgs
import qualified Data.Map as Map

main :: IO ()
main = do
  test1
  test2
  test3

test1 :: IO ()
test1 = do
    x <- withNixRepl $ \nixRepl -> do
        seven <- bindVar nixRepl (3 + 4)
        eval nixRepl $ Pkgs.map (+ seven) (toNix [1 .. 10])
    x @=? ((+ 7) <$> [1 .. 10] :: [Int])

test2 :: IO ()
test2 = do
  x <- withNixRepl $ \nixRepl-> eval nixRepl $ lets
    (mdo
      seven <- ew $ y + 1
      y <- ew 6
      pure seven
    ) (+ 3)
  x @=? (10 :: Int)

test3 :: IO ()
test3 = do
  x <- evalVar (toNix m1 Pkgs.// toNix m2)
  x @=? (m1 <> m2)
  where
    m1, m2 :: Map.Map String String
    m1 = Map.singleton "foo" "bar"
    m2 = Map.singleton "baz" "quux"
