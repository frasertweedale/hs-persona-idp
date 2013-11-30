-- This file is part of persona-idp - Persona (BrowserID) Identity Provider
-- Copyright (C) 2013  Fraser Tweedale
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

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Network.Wai
import Options.Applicative
import Text.Blaze.Renderer.Text
import Text.Hamlet
import Text.Julius
import Web.Scotty

import Crypto.Persona

import Command
import Config

data ServeOpts = ServeOpts Int

instance Command ServeOpts where
  parser = ServeOpts
    <$> option
      ( long "port"
      <> value 3000
      <> showDefault
      <> metavar "N"
      <> help "Port on which to listen"
      )
  run (ServeOpts port) = scotty port $ do
    get "/support" $ do
      supportDoc <- liftIO $ readConfig "browserid"
      raw supportDoc
      setHeader "Content-Type" "application/json; charset=UTF-8"

    get "/authentication" $ do
      req <- request
      html $ renderMarkup $(shamletFile "src/authentication.hamlet")

    get "/authentication.js" $ do
      req <- request
      let template = $(juliusFile "src/authentication.julius")
      text $ renderJavascriptUrl (\_ _ -> undefined) template
      setHeader "Content-Type" "text/javascript; charset=UTF-8"

    get "/provisioning" $ do
      req <- request
      html $ renderMarkup $(shamletFile "src/provisioning.hamlet")

    get "/provisioning.js" $ do
      req <- request
      let template = $(juliusFile "src/provisioning.julius")
      text $ renderJavascriptUrl (\_ _ -> undefined) template
      setHeader "Content-Type" "text/javascript; charset=UTF-8"

    post "/provisioning" $ do
      -- return signed application/jwt if authenticated
      -- obj <- jsonData
      -- json $ certification
      bod <- body
      liftIO $ L.putStrLn bod
      html "<h1>certify</h1>"


appRoot :: Request -> T.Text
appRoot = T.intercalate "/" . init . pathInfo

appRel :: T.Text -> Request -> T.Text
appRel s req = appRoot req `T.append` ('/' `T.cons` T.dropWhile (== '/') s)
