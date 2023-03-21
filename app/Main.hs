{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Lib
import Options.Applicative
import System.Directory
import System.Process
import GHC.IO.Handle (hFlush)
import GHC.IO.StdHandles (stdout)

{- Parsing Arguments -}
data Commands
  = List {listarg :: String}
  | Insert {addarg :: String, dir :: FilePath}
  | Init {uid :: String, path :: String}
  | Get {uid :: String, path :: String}
  | Del {uid :: String, path :: String }

newtype Options = Options {commandarg :: Commands}

listP :: Parser Commands
listP =
  List
    <$> strArgument
      (help "Path to the .password-store directory" <> metavar "STORE_PATH" <> value "HOME_FOLDER_PLACEHOLDER")

addP :: Parser Commands
addP =
  Insert
    <$> strArgument
      (help "Username to make an entry for" <> metavar "USERNAME")
    <*> strOption (help "Path to the .password-store directory" <> long "path" <> short 'p' <> metavar "STORE_PATH" <> value "HOME_FOLDER_PLACEHOLDER")

getP :: Parser Commands
getP =
  Get
    <$> strArgument
      (help "Username get the entry of" <> metavar "ENTRY")
    <*> strOption (help "Path to the .password-store directory" <> long "path" <> short 'p' <> metavar "STORE_PATH" <> value "HOME_FOLDER_PLACEHOLDER")

initP :: Parser Commands
initP =
  Init
    <$> strArgument
      (help "Username to init the store with" <> metavar "USERNAME")
    <*> strOption (help "Path to the .password-store directory" <> long "path" <> short 'p' <> metavar "STORE_PATH" <> value "HOME_FOLDER_PLACEHOLDER")


delP :: Parser Commands
delP =
  Del
    <$> strArgument
      (help "Username get the entry of" <> metavar "ENTRY")
    <*> strOption (help "Path to the .password-store directory" <> long "path" <> short 'p' <> metavar "STORE_PATH" <> value "HOME_FOLDER_PLACEHOLDER")


commandP :: Parser Options
commandP = Options <$> subcommandP

subcommandP :: Parser Commands
subcommandP =
  subparser
    ( command "list" (info listP (progDesc "list the current entries"))
        <> command "insert" (info addP (progDesc "insert a new entry into the password store"))
        <> command "init" (info initP (progDesc "initialize a new store"))
        <> command "get" (info getP (progDesc "get an entry from the password store"))
        <> command "rm" (info delP (progDesc "delete an entry from the password store"))
    )

main :: IO ()
main = do
  passwordMg =<< execParser opts
  where
    opts =
      info
        (commandP <**> helper)
        (fullDesc <> progDesc "LambdaLock")

{- The Real Main Function -}
passwordMg :: Options -> IO ()
passwordMg (Options (List dir)) = do
  if dir == "HOME_FOLDER_PLACEHOLDER"
    then do
      homedir <- getHomeDirectory
      printPasswords (homedir ++ passwordStore)
    else printPasswords dir
--
passwordMg (Options (Insert username path)) = do
  if path == "HOME_FOLDER_PLACEHOLDER"
    then do
      homedir <- getHomeDirectory
      setCurrentDirectory (homedir ++ passwordStore)
      writePass username
    else do
      setCurrentDirectory path
      writePass username
-- 
passwordMg (Options (Init key path)) = do
  if path == "HOME_FOLDER_PLACEHOLDER"
    then do
      homedir <- getHomeDirectory
      setCurrentDirectory homedir 
      createDirectory ".password-store"
      setCurrentDirectory (homedir ++ passwordStore)
      writeFile ".gpg-id" key
    else do
      setCurrentDirectory path 
      writeFile ".gpg-id" key
--
passwordMg (Options (Get key path)) = do
  if path == "HOME_FOLDER_PLACEHOLDER"
    then do
      homedir <- getHomeDirectory
      setCurrentDirectory (homedir ++ passwordStore)
      getPass key 
    else do
      setCurrentDirectory path
      getPass key
-- 
passwordMg (Options (Del key path)) = do 
  if path == "HOME_FOLDER_PLACEHOLDER"
    then do
      homedir <- getHomeDirectory
      setCurrentDirectory (homedir ++ passwordStore)
      removeFile $ key ++ ".gpg"
    else do
      setCurrentDirectory path
      removeFile $ key ++ ".gpg"


writePass :: FilePath -> IO ()
writePass filename = do
  content <- readFile ".gpg-id"
  let userid = stripEscapes content
  putStr "Enter Password: "
  hFlush stdout
  password <- getLine
  writeFile filename password
  callCommand $ "gpg -r " ++ userid ++ " --encrypt " ++ filename
  removeFile filename

getPass :: String -> IO ()
getPass uid = do
  callCommand $ "gpg --decrypt " ++ uid ++ ".gpg"
