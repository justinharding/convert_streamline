module Main where

type Transaction = (String, String, String, String)

main :: IO ()
main = do
  transactions <- myReadFile "nz.journal"
  print transactions

myReadFile :: FilePath -> IO Transaction
myReadFile path = do
  alldata <- readFile path
  return (alldata, "", "", "")


