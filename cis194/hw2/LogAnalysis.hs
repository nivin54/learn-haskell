{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Give idea on recursive data types, enumeration types, algebraic data types,
-- and pattern matching. TODO(nivin): Add an example for case expressions.
-- Example to define a local function or morphism with where condition.
parseMessage :: String -> LogMessage
parseMessage line = analyzer (words line)
  where
    analyzer ("I":timestamp:s) = LogMessage Info (read timestamp) (unwords s)
    analyzer ("W":timestamp:s) = LogMessage Warning (read timestamp) (unwords s)
    analyzer ("E":priority:timestamp:s) =
      LogMessage (Error (read priority)) (read timestamp) (unwords s)
    analyzer s = Unknown (unwords s)

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert lm Leaf = Node Leaf lm Leaf
insert (Unknown _) tree = tree
insert p@(LogMessage _ t1 _) (Node left m@(LogMessage _ t2 _) right)
  | t1 < t2 = Node (insert p left) m right
  | otherwise = Node left m (insert p right)
insert _ (Node _ (Unknown _) _) = error "Unknwon messages are not allowed."

-- foldr evaluates from right to left.
-- foldr is accumlator that takes a list of elements and process them from
-- right to left.
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

-- m : whatWentWrong xs and [m] ++ whatWentWrong xs are syntactically same.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong (LogMessage (Error value) _ m:xs)
  | value > 50 = m : whatWentWrong xs
  | otherwise = whatWentWrong xs
whatWentWrong (Unknown _:xs) = whatWentWrong xs
whatWentWrong (LogMessage _ _ _:xs) = whatWentWrong xs
whatWentWrong [] = []
