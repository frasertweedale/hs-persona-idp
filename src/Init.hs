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

module Init where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import System.Exit
import System.Directory
import System.FilePath.Posix

import Options.Applicative

import Crypto.JOSE.JWK
import Crypto.Persona

import Command

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
      (pub, sec) <- genRSA 512

      auth <- maybe exitFailure return $ buildURI "authentication"
      prov <- maybe exitFailure return $ buildURI "provisioning"
      let supportDoc = SupportDocument pub auth prov

      personaDir <- getAppUserDataDirectory "persona-idp"
      createDirectoryIfMissing False personaDir
      L.writeFile (personaDir </> "rsa.pub.json") $ encode pub
      L.writeFile (personaDir </> "rsa.sec.json") $ encode sec
      L.writeFile (personaDir </> "browserid") $ encode supportDoc
