-- This file is part of persona-idp - Persona (BrowserID) Identity Provider
-- Copyright (C) 2015  Fraser Tweedale
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

module Middleware
  (
    remoteUserX509Middleware
  , certLookupEmail
  ) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid

import Data.ASN1.Types.String
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.PEM
import Data.X509
import Network.HTTP.Types
import Network.Wai

data RemoteUserLookup = RemoteUserLookup
  HeaderName
  (B.ByteString -> Maybe B.ByteString)

remoteUserLookupX509 :: RemoteUserLookup
remoteUserLookupX509 = RemoteUserLookup "SSL_CLIENT_CERT" f
  where
  f = either (const Nothing) getEmail
    . (decodeSignedCertificate <=< content <=< pemParseBS . B.map tabToLf)
  getEmail = fmap (T.encodeUtf8 . T.pack) . certLookupEmail . getCertificate
  content [] = Left "nothing to see here"
  content (x:_) = Right (pemContent x)
  tabToLf 9 = 10
  tabToLf c = c

certLookupEmail :: Certificate -> Maybe String
certLookupEmail a = sanEmail a <|> sdnEmail a
  where
  sdnEmail = asn1CharacterToString
    <=< lookup [1,2,840,113549,1,9,1] . getDistinguishedElements . certSubjectDN
  sanEmail = findEmailAltName <=< extensionGet . certExtensions
  findEmailAltName (ExtSubjectAltName names) =
    getFirst (foldMap (First . rfc822AltName) names)
  rfc822AltName (AltNameRFC822 s) = Just s
  rfc822AltName _ = Nothing

lookupRemoteUser :: RemoteUserLookup -> Request -> Maybe B.ByteString
lookupRemoteUser (RemoteUserLookup k f) =
  f <=< lookup k . requestHeaders

-- | Create a 'Middleware' from a 'RemoteUserLookup'
--
-- The middleware triggers if the request does not contain the
-- "REMOTE_USER" header.  The first successful lookup causes the
-- result to be stored in this header, so subsequent middlewares
-- will not have effect.
--
-- Note that header names are case-insensitive.
--
remoteUserLookupMiddleware :: RemoteUserLookup -> Middleware
remoteUserLookupMiddleware a = (. modreq)
  where
  modreq req =
    let
      hs = requestHeaders req
      k = "REMOTE_USER"
    in
      case lookup k hs of
        Just _ -> req
        Nothing -> case lookupRemoteUser a req of
          Nothing -> req
          Just s -> req { requestHeaders = (k, s) : hs }

-- | Copies email address from a PEM-encoded X.509 certificate
-- provided via the SSL_CLIENT_CERT header.  The Subject Alternative
-- Name extension is preferred over the (deprecated) Subject DN
-- email component.
--

remoteUserX509Middleware :: Middleware
remoteUserX509Middleware =
  remoteUserLookupMiddleware remoteUserLookupX509
