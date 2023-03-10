{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib (main) where

import Data.ByteString.Streaming.HTTP (
    Response (responseBody),
    newManager,
    parseRequest,
    tlsManagerSettings,
    withHTTP,
 )
import Streaming.ByteString qualified as Q

import Control.Monad.State.Strict (
    MonadState (get, put),
    StateT,
    evalStateT,
 )
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.JsonStream.Parser (
    ParseOutput (..),
    Parser,
    arrayOf,
    runParser,
    value,
 )
import Streaming (
    MFunctor (hoist),
    MonadTrans (lift),
    Of,
    Stream,
    distribute,
 )
import Streaming.Prelude qualified as S

parseJson ::
    forall m a.
    Monad m =>
    Parser a ->
    Stream (Of ByteString) m () ->
    Stream (Of a) m ()
parseJson parser =
    flip evalStateT (runParser parser)
        . distribute
        . flip S.for oneChunk
        . hoist lift

oneChunk :: Monad m => ByteString -> Stream (Of a) (StateT (ParseOutput a) m) ()
oneChunk x = do
    let loop p = case p of
            ParseDone r -> case r of
                "" -> return ()
                _ -> error "parseJson: leftover"
            ParseFailed e -> error e
            ParseYield a c -> S.yield a >> loop c
            ParseNeedData c -> put $ c x
    get >>= loop

main :: String -> (Stream (Of Value) IO () -> IO ()) -> IO ()
main target what = do
    req <- parseRequest target
    m <- newManager tlsManagerSettings
    withHTTP req m $ \resp ->
        what $
            parseJson (arrayOf (value @Value)) $
                Q.toChunks (responseBody resp)