module Types where

data Request = Request
               {rqYear::Int
               , rqMonth::Int
               , rqDay::Int -- Jan = 1
               , rqUrl::String
               } deriving (Show)
                          
