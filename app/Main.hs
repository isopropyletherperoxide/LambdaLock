{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Options.Applicative
import System.Directory
import System.FilePath.Posix

{- Parsing Arguments -}

data Commands = List {listarg :: String} | Insert {addarg :: String, dir :: FilePath}

data Options = Options {commandarg :: Commands}

listP :: Parser Commands
listP = List <$> strArgument (help "Path to the .password-store directory" <> metavar "Store Directory" <> value "HOME_FOLDER_PLACEHOLDER")

addP :: Parser Commands
addP = Insert <$> strArgument (help "bingle" <> metavar "Username") <*> strOption (long "path" <> short 'p' <> metavar "Storage Path" <> value "HOME_FOLDER_PLACEHOLDER")

commandP :: Parser Options
commandP = Options <$> subcommandP

subcommandP :: Parser Commands
subcommandP =
  subparser
    ( command "list" (info listP (progDesc "list"))
        <> command "insert" (info addP (progDesc "insert"))
    )

main :: IO ()
main = do
  passwordMg =<< execParser opts
  where
    opts =
      info
        (commandP <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

{- The Real Main Function -}

passwordMg :: Options -> IO ()
passwordMg (Options (List d)) = do
  if d == "HOME_FOLDER_PLACEHOLDER"
    then do
      a <- getHomeDirectory
      printPasswords (a ++ passwordStore)
    else printPasswords d
--
passwordMg (Options (Insert d path)) = do
  if path == "HOME_FOLDER_PLACEHOLDER"
    then do
      a <- getHomeDirectory
      setCurrentDirectory (a ++ passwordStore)
      password <- getLine
      writeFile d password
    else do
      setCurrentDirectory path
      password <- getLine
      writeFile d password

