{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (tryJust)
import Control.Monad (guard, unless)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (showGregorian, toGregorian, Day)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, localDay)
import System.IO (readFile)
import System.IO.Error (isDoesNotExistError)
import Text.Editor (runUserEditorDWIMFile, markdownTemplate, wrapStr)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)

planDir = "/Users/chadknight/.dayplan/plans/"

main :: IO ()
main = do
  today <- getToday
  let path = makePath today
  mkDirP path
  let fileName = makeFileName today
  putStrLn $ "working with " ++ fileName
  writeFile fileName $ makePrompt today
  stuff <- fmap wrapStr (runUserEditorDWIMFile markdownTemplate fileName)
  putStrLn stuff
  writeFile fileName stuff

getToday :: IO Day
getToday = do
  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  return $ localDay zoneNow

mkDirP :: FilePath -> IO FilePath
mkDirP path = do
  ex <- doesDirectoryExist path
  unless ex (createDirectoryIfMissing True path)
  return path

getMessage :: String -> IO String
getMessage fileName = do
  f <- tryJust (guard . isDoesNotExistError) $ readFile fileName
  return $ either (const "You didn't make a plan for yesterday.") id f

showDay :: Day -> String
showDay day = formatTime defaultTimeLocale "%a %b %d, %Y" day

makeFileName :: Day -> String
makeFileName day = (makePath day) ++ (showGregorian day) ++ ".plan"

makePath :: Day -> String
makePath day = planDir ++ formatTime defaultTimeLocale "%Y/%m/" day

makePrompt :: Day -> String
makePrompt day =
  dayStr ++ "\n" ++ (duplicate "-" $ length dayStr)
  where dayStr = showDay day

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

