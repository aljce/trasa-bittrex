{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
-- |

module Bittrex.Money where

import           Data.Foldable (find)
import           Data.Kind     (Type)
import           Data.Proxy    (Proxy (..))
import           Data.Void     (Void)
import           GHC.Generics
import           GHC.TypeLits  (KnownSymbol, SomeSymbol (..), Symbol, symbolVal)

import qualified Data.Text     as T

data Dense currency = Dense Rational

class Currency code where
  empty :: code -> Void
  default empty
    :: (Generic code, Rep code x ~ D1 m V1 x) => code -> Void
  empty code = case from code of
    !v -> error "Currency.empty: the impossible happened"

  identifier :: T.Text
  default identifier
    :: forall x d m p n v. (Generic code, KnownSymbol d, Rep code x ~ D1 ('MetaData d m p n) v x) => T.Text
  identifier = T.pack (symbolVal (Proxy @d))

instance Currency code => Show (Dense code) where
  show (Dense r) = T.unpack (identifier @code) ++ ": " ++ show r

data SomeCurrency :: Type where
  SomeCurrency :: Currency code => Proxy code -> SomeCurrency

instance Eq SomeCurrency where
  SomeCurrency (Proxy :: Proxy code1) == SomeCurrency (Proxy :: Proxy code2) =
    identifier @code1 == identifier @code2

someCurrency :: forall code. Currency code => SomeCurrency
someCurrency = SomeCurrency (Proxy @code)

withCurrencies :: Foldable f => f SomeCurrency -> T.Text -> r -> (forall code. Currency code => r) -> r
withCurrencies currencies str noMatch match = case find isMatch currencies of
  Nothing -> noMatch
  Just (SomeCurrency (Proxy :: Proxy code)) -> match @code
  where
    isMatch :: SomeCurrency -> Bool
    isMatch (SomeCurrency (Proxy :: Proxy code)) = identifier @code == str

data USD
  deriving Generic

instance Currency USD where

data ADA
  deriving Generic

instance Currency ADA where

data EUR
  deriving Generic

instance Currency EUR where

providedCurrencies :: [ SomeCurrency ]
providedCurrencies = [ someCurrency @USD, someCurrency @ADA, someCurrency @EUR ]
