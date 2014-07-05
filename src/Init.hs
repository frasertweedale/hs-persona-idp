-- This file is part of persona-idp - Persona (BrowserID) Identity Provider
--
-- Copyright (C) 2013, 2014  Fraser Tweedale
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

module Init where

import System.Exit

import Options.Applicative

import Crypto.JOSE
import Crypto.JOSE.Legacy
import Crypto.Persona

import Command
import Config

data InitOpts = InitOpts String

instance Command InitOpts where
  parser = InitOpts
    <$> strOption
      ( long "app-path"
      <> metavar "PATH"
      <> help "Path at which the app is hosted, e.g. \"/browserid\""
      )
  run (InitOpts appPath) =
    let
      buildURIPath s = '/' : dropWhile (== '/') (appPath ++ "/" ++ s)
      buildURI = parseRelativeURI . buildURIPath
    in do
      entropyPool <- createEntropyPool
      let g = cprgCreate entropyPool :: SystemRNG
          k = genRSA' g 256  -- jwcrypto does not support keys > 2048 bits
      auth <- maybe exitFailure return $ buildURI "authentication"
      prov <- maybe exitFailure return $ buildURI "provisioning"
      ensureConfigDir
      writeConfigJSON "rsa.json" k
      -- TODO extract public key from private key !!!
      writeConfigJSON "browserid" $ SupportDocument k auth prov
