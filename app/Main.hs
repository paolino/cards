{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Monad ((<=<))
import Lib ( run )
import Streaming.Prelude qualified as S
import System.Environment (getArgs)

main :: IO ()
main = do
    (target : _) <- getArgs
    run target $ print <=< S.length_
