{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( filterPasswords, trimPasswords, prettyPrintFilename, prettyPrintFilename1, listPasswords, passwordStore, printPasswords
    ) where

import Data.Text (pack, replace, unpack)
import Data.List (isPrefixOf)
import System.Directory

-- | Removes all the hidden files from the password folder
filterPasswords :: [FilePath] -> [FilePath]
filterPasswords = filter (not . isPrefixOf ".")

-- | Trims the filetype from the password 
trimPasswords :: [FilePath] -> [FilePath]
trimPasswords = map ((unpack . replace ".gpg" "") . pack)

-- | Pretty prints a filename 
prettyPrintFilename :: FilePath -> IO ()
prettyPrintFilename filename = putStrLn ("├── " ++ filename)

-- | Pretty prints the terminal filename 
prettyPrintFilename1 :: FilePath -> IO ()
prettyPrintFilename1 filename = putStrLn ("└── " ++ filename)

-- | Prints all the passwords 
printPasswords :: FilePath -> IO ()
printPasswords dir = do
  b <- listDirectory dir
  listPasswords b

-- | The Default filepath to a password store
passwordStore :: FilePath
passwordStore = "/.password-store"

-- | Lists all the passwords 
listPasswords :: [FilePath] -> IO ()
listPasswords dir = do
  putStrLn "Password Store"
  mapM_ prettyPrintFilename dirlist
  prettyPrintFilename1 $ last (trimPasswords $ filterPasswords dir)
  where
    dirlist = trimPasswords $ filterPasswords (take (length dir - 1) dir)
