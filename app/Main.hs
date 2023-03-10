{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Monad ((<=<))
import Lib qualified
import Streaming.Prelude qualified as S
import System.Environment (getArgs)

main :: IO ()
main = do
    (target : _) <- getArgs
    Lib.main target $ print <=< S.length_
