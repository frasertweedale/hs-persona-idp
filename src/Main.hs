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

import Options.Applicative

import Command
import Init
import Serve

data Opts
  = Init InitOpts
  | Serve ServeOpts

instance Command Opts where
  parser = subparser
    ( command "init" (info (Init <$> parser <**> helper)
      ( progDesc "Generate keys and static files"
      ))
    <> command "serve" (info (Serve <$> parser <**> helper)
      ( progDesc "Run the identity provider"
      ))
    )
  run (Init a) = run a
  run (Serve a) = run a

main :: IO ()
main = execParser (info (helper <*> (parser :: Parser Opts))
  ( progDesc "persona - Persona (BrowserID) Identity Provider"
  )) >>= run
