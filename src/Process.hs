module Process where

import Data.Map.Strict as M
import Prelude as P
--import Data.Set as S


import Types

--count' :: 
--count' m ([]) = m
count' m x =
  let c = M.findWithDefault 0 x m in
  insert x (1+c) m




count = P.foldl count' M.empty 

p1 = count [(2015, 10), (2015, 11), (2014, 10), (2015,10)]


countReqs :: [Request] -> [((Int, Int), Int)]
countReqs reqs =
  toList counts
  where
    yrMths = P.map (\r -> ( rqYear r, rqMonth r)) reqs
    counts = count yrMths
    

