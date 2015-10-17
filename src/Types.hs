module Types where

data Request = Request
               {rqYear::Int
               , rqMonth::Int
               , rqDay::Int -- Jan = 1
               , url::String
               } deriving (Show)
                          
