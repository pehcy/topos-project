module FileIO where

import Data.ByteString (Char8)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)

import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)

{- File IO for data mining
 - Tasks:
 1. Read CSV/text/SQL database
 2. Create simple numerical calculation on data
-}

-- putStr pack "file" =~ ".extension" :: [ByteString]

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pattern
  | not (isPattern pattern) = do
    exists <- doesNameExist pattern
    return (if exists then [pattern] else [])
  | otherwise = do
    case splitFileName pattern of
      ("", baseName) -> do
        curDir <- getCurrentDirectory
        listMatches curDir baseName
      (dirName, baseName) -> do
        dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
        let listDir = if isPattern baseName 
                      then listMatches 
                      else listPlain
        pathNames <- forM dirs $ \dir -> do 
                      baseNames <- listDir dir baseName
                      return (map (dir </>) baseNames)
        return (concat pathNames) 

doesNameExist :: FilePath -> IO Bool 

doesNameExist fname = do 
  fileExists <- doesFileExist name
  if fileExists then return True else doesDirectoryExist name
