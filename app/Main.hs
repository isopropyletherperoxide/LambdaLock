{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.Text
import GHC.IO.Handle (hFlush)
import GHC.IO.StdHandles (stdout)
import Lib
import Options.Applicative
import System.Directory
import System.Process (callCommand)

{- Parsing Arguments -}
data Commands
  = List {listarg :: Maybe FilePath}
  | Insert {addarg :: Text, dir :: Maybe FilePath}
  | Init {uid :: Text, path :: Maybe FilePath }
  | Get {uid1 :: Text, path1 :: Maybe FilePath}
  | Del {uid2 :: Text, path2 :: Maybe FilePath}


newtype Options = Options {commandarg :: Commands}

listP :: Parser Commands
listP =
  List
    <$> optional
      ( strArgument
          (help "Path to the .password-store directory" <> metavar "STORE_PATH")
      )

addP :: Parser Commands
addP =
  Insert
    <$> strArgument
      (help "Username to make an entry for" <> metavar "USERNAME")
    <*> optional (strOption (help "Path to the .password-store directory" <> long "path" <> short 'p' <> metavar "STORE_PATH"))

getP :: Parser Commands
getP =
  Get
    <$> strArgument
      (help "Username get the entry of" <> metavar "ENTRY")
    <*> optional (strOption (help "Path to the .password-store directory" <> long "path" <> short 'p' <> metavar "STORE_PATH"))

initP :: Parser Commands
initP =
  Init
    <$> strArgument
      (help "Username to init the store with" <> metavar "USERNAME")
    <*> optional (strOption (help "Path to the .password-store directory" <> long "path" <> short 'p' <> metavar "STORE_PATH"))

delP :: Parser Commands
delP =
  Del
    <$> strArgument
      (help "Username get the entry of" <> metavar "ENTRY")
    <*> optional (strOption (help "Path to the .password-store directory" <> long "path" <> short 'p' <> metavar "STORE_PATH"))

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
  case dir of
    Nothing -> do
      homedir <- getHomeDirectory
      printPasswords (homedir ++ passwordStore)
    Just a -> printPasswords a
--
passwordMg (Options (Insert username path)) = do
  case path of
    Nothing -> do
      homedir <- getHomeDirectory
      setCurrentDirectory (homedir ++ passwordStore)
      writePass $ unpack username
    Just path' -> do
      setCurrentDirectory path'
      writePass $ unpack username
--
passwordMg (Options (Init key path)) = do
  case path of
    Nothing -> do
      homedir <- getHomeDirectory
      setCurrentDirectory homedir
      createDirectory ".password-store"
      setCurrentDirectory (homedir ++ passwordStore)
      writeFile ".gpg-id" (unpack key)
    Just path' -> do
      setCurrentDirectory path'
      writeFile ".gpg-id" (unpack key)
--
passwordMg (Options (Get key path)) = do
  case path of
    Nothing -> do
      homedir <- getHomeDirectory
      setCurrentDirectory (homedir ++ passwordStore)
      getPass $ unpack key
    Just path' -> do
      setCurrentDirectory path'
      getPass $ unpack key
--
passwordMg (Options (Del key path)) = do
  case path of
    Nothing -> do
      homedir <- getHomeDirectory
      setCurrentDirectory (homedir ++ passwordStore)
      removeFile $ unpack key ++ ".gpg"
    Just path' -> do
      setCurrentDirectory path'
      removeFile $ unpack key ++ ".gpg"

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
