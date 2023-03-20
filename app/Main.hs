{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Options.Applicative
import System.Directory
import System.Process
import Data.Char (isSpace)


{- Parsing Arguments -}

data Commands = List {listarg :: String} | Insert {addarg :: String, dir :: FilePath} | Init {uid :: String, path :: String} 

data Options = Options {commandarg :: Commands}

listP :: Parser Commands
listP = List <$> strArgument 
        (help "Path to the .password-store directory" <> metavar "Store Directory" <> value "HOME_FOLDER_PLACEHOLDER")

addP :: Parser Commands
addP = Insert <$> strArgument 
        (help "Username to make an entry for" <> metavar "Username")
        <*> strOption (long "path" <> short 'p' <> metavar "Storage Path" <> value "HOME_FOLDER_PLACEHOLDER")


initP :: Parser Commands
initP = Insert <$> strArgument 
        (help "Username to init the store with" <> metavar "Username")
        <*> strOption (long "path" <> short 'p' <> metavar "Storage Path" <> value "HOME_FOLDER_PLACEHOLDER")

commandP :: Parser Options
commandP = Options <$> subcommandP

subcommandP :: Parser Commands
subcommandP =
  subparser
    ( command "list" (info listP (progDesc "list the current entries"))
        <> command "insert" (info addP (progDesc "insert a new entry into the password store"))
        <> command "init" (info initP (progDesc "initialize a new store"))
    )

main :: IO ()
main = do
  passwordMg =<< execParser opts
  where
    opts =
      info
        (commandP <**> helper)
        ( fullDesc
            <> progDesc "LambdaLock"
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
      writePass d 
    else do
      setCurrentDirectory path
      writePass d 
-- 
passwordMg (Options (Init key path)) = do
        if path == "HOME_FOLDER_PLACEHOLDER"
           then do 
                   print key
           else do 
                   print path



writePass :: FilePath -> IO () 
writePass filename = do
      content <- readFile ".gpg-id"
      let userid = stripEscapes content
      print userid
      putStr "Enter Password: "
      password <- getLine
      writeFile filename password
      callCommand $ "gpg -r " ++ userid ++ " --encrypt " ++ filename 
      removeFile filename

stripEscapes :: [Char] -> [Char]
stripEscapes = reverse . dropWhile isSpace . reverse

