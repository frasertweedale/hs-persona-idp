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

module Config
  (
    ensureConfigDir
  , readConfig
  , readConfigJSON
  , writeConfig
  , writeConfigJSON
  ) where

import Control.Monad

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import System.FilePath.Posix
import System.Directory
import System.Posix.Files (setFileCreationMask)


personaDir :: IO String
personaDir = getAppUserDataDirectory "persona-idp"

ensureConfigDir :: IO ()
ensureConfigDir = personaDir >>= createDirectoryIfMissing False

readConfig :: String -> IO L.ByteString
readConfig n = personaDir >>= L.readFile . (</> n)

readConfigJSON :: FromJSON a => String -> IO (Either String a)
readConfigJSON = liftM eitherDecode . readConfig

writeConfig :: String -> L.ByteString -> IO ()
writeConfig n s = do
  _ <- setFileCreationMask 0o0077
  dir <- personaDir
  L.writeFile (dir </> n) s

writeConfigJSON :: ToJSON a => String -> a -> IO ()
writeConfigJSON s = writeConfig s . encode
