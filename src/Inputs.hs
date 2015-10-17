module Inputs where

--import Data.ByteString.Internal as I
--import Data.ByteString as B
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as L
import qualified Codec.Compression.GZip as GZip
import Prelude as P
import System.Directory
import System.Path.Glob


import Parser
import Types


root = "/home/mcarter/cubie/var/log/nginx/access.log"

inputGz :: String -> IO [Request]
inputGz fileName = do
  --let file = "/home/mcarter/cubie/var/log/nginx/access.log.23.gz"
  c1 <- fmap GZip.decompress (L.readFile fileName)
  let c2 = L.toStrict c1
  let c3 = C.unpack c2
  -- return c3
  return $ parseStr fileName c3

i1 = inputGz "/home/mcarter/cubie/var/log/nginx/access.log.23.gz"

allGz = do
  c1 <- glob $ root ++ "*.gz"
  c2 <- P.mapM inputGz c1
  let c3 = P.concat c2
  --c3 <- c2
  --let c3 = P.concat c2
  return c3



inputTxt :: String -> IO [Request]
inputTxt fileName = do
  c1 <- P.readFile fileName
  return $ parseStr fileName c1

inputAll :: IO [Request]  
inputAll = do
  i1 <- inputTxt root
  let dot1 = root ++ ".1"
  i2 <- inputTxt dot1
  i3 <- allGz
  let all = i1 ++ i2 ++ i3
  return all
