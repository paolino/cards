{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Monad
import Lib qualified
import Streaming.Prelude qualified as S

target :: String
target = "https://data.scryfall.io/all-cards/all-cards-20230310101654.json"

main :: IO ()
main = Lib.main target (print <=< S.length_)
