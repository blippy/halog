module Main where

--import Data.List
import GHC.Exts
import Text.Printf

import Inputs
import Process
import Types
--import Parser



fmtYMcount :: ((Int,Int), Int) -> String
fmtYMcount  ymc =
  let ((y, m), c) = ymc in
  printf "%4d-%02d %9d"  y m c

main :: IO ()

reportReqs:: [Request] -> IO ()
reportReqs reqs= do
  --reqs <- inputTxt root
  let counts = countReqs reqs
  let outLines = map fmtYMcount counts
  writeFile "/home/mcarter/.halog/ymc.txt" $ unlines outLines

fmtUrl :: (String, Int) -> String
fmtUrl (u, c) = printf "%9d %s" c u

reportUrls:: [Request] -> IO ()
reportUrls reqs = do
  let counts = reverse $ sortWith snd $ countUrls reqs
  let outLines = map fmtUrl counts
  writeFile "/home/mcarter/.halog/urls.txt" $ unlines outLines

test1 = do
  reqs <-inputTxt root
  reportReqs reqs

main = do
  reqs <- inputAll
  reportReqs reqs
  reportUrls reqs
  print "Finished"
