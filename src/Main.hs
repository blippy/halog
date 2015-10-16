module Main where

import Parser

main :: IO ()
main = do
  logFile <- readFile "/home/mcarter/cubie/var/log/nginx/access.log"
  let res = parseStr  logFile
  putStrLn $ show res
