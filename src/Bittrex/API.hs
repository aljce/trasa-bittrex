{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeInType        #-}
module Bittrex.API where

import           Data.Bifunctor      (first)
import           Data.Kind           (Type)

import           Data.Aeson          (FromJSON, ToJSON, eitherDecode', encode)
import qualified Data.Text           as T
import qualified Data.Vector         as V

import qualified Network.HTTP.Client as HTTP
import           Trasa.Core
import           Trasa.Core.Implicit

import           Bittrex.Types

bodyAeson :: FromJSON a => BodyDecoding a
bodyAeson = BodyDecoding (pure "application/json") (first T.pack . eitherDecode')

api :: Path capCodec caps -> Path capCodec caps
api rest = match "api" ./ match "v1.1" ./ match "public" ./ rest

data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
  Markets :: Route '[] '[] Bodyless (BittrexResult (V.Vector SomeMarket))

instance HasMeta Route where
  type CaptureStrategy Route = CaptureEncoding
  type QueryStrategy Route = CaptureEncoding
  type RequestBodyStrategy Route = Many BodyEncoding
  type ResponseBodyStrategy Route = Many BodyDecoding
  meta route = metaBuilderToMetaCodec $ case route of
    Markets -> Meta (api ./ match "getmarkets" ./ end) qend bodyless (resp bodyAeson) "GET"

bittrex :: Auth -> HTTP.Manager -> Prepared Route (BittrexResult response) -> IO (BittrexResult response)
bittrex auth mgr route = undefined
  where
    toMeta = undefined
    run = undefined
