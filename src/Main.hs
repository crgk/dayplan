{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (tryJust)
import Control.Monad (guard, unless, when, void)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeOrError)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (showGregorian, toGregorian, Day)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, localDay)
import System.Environment (getArgs)
import System.IO (readFile)
import System.IO.Error (isDoesNotExistError)
import Text.Editor (runUserEditorDWIMFile, markdownTemplate, wrapStr)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, doesFileExist)

dayplanPath = "/Users/chadknight/.dayplan"
plansDir = dayplanPath ++ "/plans"
planListPath = dayplanPath ++ "/plan.list"

main :: IO ()
main = do
  args <- getArgs

  -- make the path to today's plan
  today <- getToday
  let path = makePath today

  -- make the path to the file
  -- (only does something for new months)
  mkDirP path

  -- make the absolute path for today's plan
  let fileName = makeFileName today

  -- prep the file, unless already done
  ex <- doesFileExist fileName
  unless ex (writeFile fileName $ initPlanText (makeHeader today))

  -- show the last plan, if in review mode
  lastPlannedDay <- getLastPlannedDay
  let lastFileName = makeFileName lastPlannedDay
  lastPlan <- getPlan lastFileName
  when (elem "-review" args) (void $ runUserEditorDWIMFile lastPlan lastFileName)

  -- prompt the user for a plan, or review an existing plan
  plan <- fmap wrapStr (runUserEditorDWIMFile markdownTemplate fileName)

  -- save the plan
  writeFile fileName plan

  -- record that we planned something for today
  addDayToList today

  -- print the plan to confirm
  putStrLn plan

addDayToList :: Day -> IO ()
addDayToList day = do
  lastPlannedDay <- getLastPlannedDay
  let planned = (lastPlannedDay == day)
  unless planned (appendFile planListPath $ (showGregorian day) ++ "\n")

getLastPlannedDay :: IO Day
getLastPlannedDay = do
  content <- readFile planListPath
  let lastDayStr = last $ lines content
  return $ parseTimeOrError True defaultTimeLocale "%F" lastDayStr

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

getPlan :: String -> IO String
getPlan fileName = do
  f <- tryJust (guard . isDoesNotExistError) $ readFile fileName
  return $ either (const "not found") id f

showDay :: Day -> String
showDay day = formatTime defaultTimeLocale "%a %b %d, %Y" day

makeFileName :: Day -> String
makeFileName day = (makePath day) ++ (showGregorian day) ++ ".plan"

makePath :: Day -> String
makePath day = plansDir ++ formatTime defaultTimeLocale "/%Y/%m/" day

initPlanText :: String -> String
initPlanText header =
  header ++ "\n"

makeHeader :: Day -> String
makeHeader day =
  dayStr ++ "\n" ++ (duplicate "-" $ length dayStr) ++ "\n"
  where dayStr = showDay day

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

