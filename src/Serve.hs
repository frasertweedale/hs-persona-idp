-- This file is part of persona-idp - Persona (BrowserID) Identity Provider
-- Copyright (C) 2013, 2014, 2015  Fraser Tweedale
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
{-# LANGUAGE TemplateHaskell #-}

module Serve (ServeOpts) where

import Control.Monad.IO.Class
import System.Exit

import Control.Lens
import Data.Aeson (eitherDecode, toJSON)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status
import Options.Applicative hiding (header)
import Text.Blaze.Renderer.Text
import Text.Hamlet
import Text.Julius
import Web.Scotty

import Crypto.JOSE (encodeCompact)
import Crypto.JWT (fromString)
import Crypto.Persona

import Command
import Config
import Middleware
import Provision

data ServeOpts = ServeOpts Int

instance Command ServeOpts where
  parser = ServeOpts
    <$> option auto
      ( long "port"
      <> value 3000
      <> showDefault
      <> metavar "N"
      <> help "Port on which to listen"
      )
  run (ServeOpts port) = do
    let handleError = either (\e -> print e >> exitFailure) return
    delegatedSupport <- handleError =<< readConfigJSON "delegated-support.json"
    support <- handleError =<< readConfigJSON "support.json"
    k <- handleError =<< readConfigJSON "key.json"

    scotty port $ do
      middleware remoteUserX509Middleware

      get "/delegated-support" $ json delegatedSupport
      get "/support" $ json support
      get "/.well-known/browserid" $ json support

      get "/provisioning" $
        html $ renderMarkup $(shamletFile "src/provisioning.hamlet")

      get "/provisioning.js" $ do
        let template = $(juliusFile "src/provisioning.julius")
        text $ renderJavascriptUrl (\_ _ -> undefined) template
        setHeader "Content-Type" "text/javascript; charset=UTF-8"

      post "/provisioning" $ do
        provReq' <- body
        case eitherDecode provReq' of
          Left e -> respondWith badRequest400 e
          Right provReq -> do
            remoteUser <- header "REMOTE_USER"
            if remoteUser == Just (TL.fromStrict $ provReq ^. eml)
            then do
              let iss = fromString $ T.pack $ delegatedSupport ^. authority
              result <- liftIO ((>>= encodeCompact) <$> provision k iss provReq)
              case result of
                Left e -> respondWith internalServerError500 (show e)
                Right cert -> raw cert
            else
              status forbidden403

respondWith :: Status -> String -> ActionM ()
respondWith e s = status e >> text (TL.pack $ show s)
