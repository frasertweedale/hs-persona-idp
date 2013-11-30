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

module Serve (ServeOpts) where

import Control.Monad.IO.Class

import Options.Applicative
import Web.Scotty

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

    get "/authentication" $
      html "<h1>authentication</h1>"

    get "/provisioning" $
      html "<h1>provisioning</h1>"
