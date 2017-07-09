module Main where

import Data.Time.Calendar (showGregorian)
import qualified Data.Dates as Dates
import System.IO (readFile)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (tryJust)
import Control.Monad (guard)

main :: IO ()
main = do
  -- get the current date (IO from system clock?)
  today <- Dates.getCurrentDateTime
  -- figure out what the previous day was
  let lastday = getLastWorkDay today
  msg <- getMessage (makeFileName lastday)
  putStrLn $ msg

getLastWorkDay :: Dates.DateTime -> Dates.DateTime
getLastWorkDay currentDate =
  Dates.minusInterval currentDate (Dates.Days 1)

makeFileName :: Dates.DateTime -> String
makeFileName date = showGregorian (Dates.dateTimeToDay date) ++ ".plan"

getMessage :: String -> IO String
getMessage fileName = do
  f <- tryJust (guard . isDoesNotExistError) $ readFile fileName
  return $ either (const "You didn't make a plan for yesterday.") id f
