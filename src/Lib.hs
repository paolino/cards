{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib (run) where

import Data.ByteString.Streaming.HTTP
    ( Response (responseBody)
    , newManager
    , parseRequest
    , tlsManagerSettings
    , withHTTP
    )
import Streaming.ByteString qualified as Q

import Control.Monad.State.Strict
    ( MonadState (get, put)
    , StateT
    , evalStateT
    )
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.JsonStream.Parser
    ( ParseOutput (..)
    , Parser
    , arrayOf
    , runParser
    , value
    )
import Streaming
    ( MFunctor (hoist)
    , MonadTrans (lift)
    , Of
    , Stream
    , distribute
    )
import Streaming.Prelude qualified as S

-- | Parse a stream of ByteStings into values via a streaming json parser.
parseJson
    :: forall m a
     . Monad m
    => Parser a
    -- ^ Parser to use
    -> Stream (Of ByteString) m ()
    -- ^ Stream of chunks to consume
    -> Stream (Of a) m ()
    -- ^ Stream of parsed values
parseJson parser =
    flip evalStateT (runParser parser)
        . distribute
        . flip S.for oneChunk
        . hoist lift

-- | Parse a ByteString into some JSON, keeping track of the parser state.
oneChunk
    :: Monad m
    => ByteString
    -- ^ Chunk of input to consume
    -> Stream (Of a) (StateT (ParseOutput a) m) ()
    -- ^ Stream of parsed values with updated parser state
oneChunk x = do
    let loop p = case p of
            ParseDone r -> case r of
                "" -> return ()
                _ -> error "parseJson: leftover"
            ParseFailed e -> error e
            ParseYield a p' -> S.yield a >> loop p'
            ParseNeedData c -> put $ c x
    get >>= loop

run
    :: String
    -- ^ Target URL
    -> (Stream (Of Value) IO () -> IO ())
    -- ^ What to do with the stream
    -> IO ()
run target what = do
    req <- parseRequest target
    m <- newManager tlsManagerSettings
    withHTTP req m $ \resp ->
        what $
            parseJson (arrayOf (value @Value)) $
                Q.toChunks (responseBody resp) >> S.yield ""
