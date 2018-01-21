{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Bittrex.Types where

import           Data.Kind    (Type)
import           Data.Proxy   (Proxy (..))
import           GHC.Generics
import           GHC.TypeLits (KnownSymbol, SomeSymbol (..), Symbol,
                               someSymbolVal)

import           Data.Aeson   ((.:))
import qualified Data.Aeson   as A
import qualified Data.Text    as T

-- import           Trasa.Core (TrasaErr)

data Auth = Auth
  { authKey :: T.Text
  , authSecret :: T.Text
  } deriving Generic

instance A.FromJSON Auth where
  parseJSON = A.genericParseJSON (A.defaultOptions { A.fieldLabelModifier = drop 4 })

data BittrexResult a
  = BittrexResult Bool T.Text a

instance A.FromJSON a => A.FromJSON (BittrexResult a) where
  parseJSON = A.withObject "BittrexError" $ \o -> do
    success <- o .: "success"
    message <- o .: "message"
    result  <- o .: "result"
    pure (BittrexResult success message result)

data Market :: Symbol -> Symbol -> Type where
  Market :: (KnownSymbol market, KnownSymbol base) => Market market base

data SomeMarket :: Type where
  SomeMarket :: Market market base -> SomeMarket

instance A.FromJSON SomeMarket where
  parseJSON = A.withObject "SomeMarket" $ \o -> do
    marketCur <- o .: "MarketCurrency"
    baseCur   <- o .: "BaseCurrency"
    pure (market marketCur baseCur)

market :: String -> String -> SomeMarket
market m b = case someSymbolVal m of
  SomeSymbol (Proxy :: Proxy market) -> case someSymbolVal b of
    SomeSymbol (Proxy :: Proxy base) -> SomeMarket (Market @market @base)

withMarket :: SomeMarket -> (forall market base. Market market base -> r) -> r
withMarket (SomeMarket m) f = f m
