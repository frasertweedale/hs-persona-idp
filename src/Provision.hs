-- This file is part of persona-idp - Persona (BrowserID) Identity Provider
-- Copyright (C) 2014  Fraser Tweedale
--
-- persona-idp is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Provision
  (
    ProvisioningRequest(ProvisioningRequest)
  , eml
  , pub
  , dur

  , provision
  ) where

import Control.Applicative

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T

import Crypto.Random
import Crypto.JOSE
import Crypto.JOSE.Legacy
import Crypto.JWT
import Crypto.Persona

data ProvisioningRequest = ProvisioningRequest
  { _eml :: T.Text
  , _pub :: JWK'
  , _dur :: Integer
  }
makeLenses ''ProvisioningRequest

instance FromJSON ProvisioningRequest where
  parseJSON = withObject "ProvisioningRequest"$ \o -> ProvisioningRequest
    <$> o .: "eml"
    <*> o .: "pub"
    <*> o .: "dur"

provision :: ProvisioningRequest -> IO (Either Error JWT)
provision ProvisioningRequest{..} = do
  g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
  let _dur' = max _dur 86400
  return $ fst $ certify g undefined {- TODO signingKey -}
    (fromString "https://frase.id.au/")
    undefined {- expiry date 'NumericDate'; calculate from dur -}
    _pub
    (EmailPrincipal _eml)


  -- SHOULD NOT issue cert valid longer than duration
  -- MUST NOT issue cert valid longer than 24 hours
  --
  -- issuer, expiry, publickey, principal and JWT
