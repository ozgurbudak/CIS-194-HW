{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

import Data.List.Split
stringHelper :: String -> [String]
stringHelper n = splitOn " " n


concatListOfStr :: [String] -> String
concatListOfStr (x:[]) = x
concatListOfStr (x:y)= x++" "++(concatListOfStr y)

stringHelper2 :: [String] -> LogMessage
stringHelper2 ("E":num:time:str) = LogMessage (Error (read num::Int)) (read time::Int) (concatListOfStr str)
stringHelper2 ("I":time:str) = LogMessage Info (read time::Int) (concatListOfStr str)
stringHelper2 ("W":time:str) = LogMessage Warning (read time::Int) (concatListOfStr str)
stringHelper2 n = Unknown (concatListOfStr n)

parseMessage :: String -> LogMessage
parseMessage n = stringHelper2 (stringHelper n)


parse :: String -> [LogMessage]
parse = (map parseMessage) . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown x) tree = tree
insert n Leaf = Node Leaf n Leaf
insert (LogMessage x tCurr y)  (Node lTree (LogMessage k tPrev l) rTree)= 
    if tCurr >= tPrev 
        then (Node lTree (LogMessage k tPrev l) (insert (LogMessage x tCurr y) rTree))
        else (Node  (insert (LogMessage x tCurr y) lTree) (LogMessage k tPrev l) rTree) 
           


build :: [LogMessage] -> MessageTree
build (x:[])= insert x Leaf
build (x:y)= insert x (build y)

a::String
a= "I 2920 all of them bowed low."

b::String
b="I 2788 from?'"

c::String
c="I 2378 vanishing so suddenly: you make one quite giddy.'"

d::String
d="E 33 424 Rabbit returning, splendidly dressed, with a pair of white kid gloves in"

lst:: [LogMessage]
lst= [(parseMessage a),(parseMessage b),(parseMessage c),(parseMessage d)]


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf n Leaf)= [n]
inOrder (Node lTree n rTree)= (inOrder lTree) ++ [n] ++ (inOrder rTree)

wwhhelper :: [LogMessage] -> [String]
wwhhelper []= []
wwhhelper ((LogMessage (Error n) _ str):rest) = if n >= 50 then (str:(wwhhelper rest)) else (wwhhelper rest)
wwhhelper ((LogMessage _ _ _):rest)= wwhhelper rest

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong n=  wwhhelper (inOrder (build n))
