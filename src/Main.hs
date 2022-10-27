module Main
  ( module Main
  ) where

import Compiler
import Parser
import Syntax

import Control.Monad.State.Strict
import Control.Monad.Trans (lift)

import Control.Monad.Trans
import Data.Either (fromRight)
import System.Console.Haskeline

main :: IO ((), Env)
main = runStateT (runInputT defaultSettings loop) defaultEnv
  where
    loop = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          let parsed = parseToplevel input
          case parsed of
            Left err -> do
              outputStrLn $ show err
              loop
            Right ex -> do
              env <- lift get
              case evalAll ex env of
                Right (res, newEnv) -> do
                  outputStrLn $ show res
                  lift (put newEnv)
                  loop
                Left err -> do
                  outputStrLn $ show err
                  loop
