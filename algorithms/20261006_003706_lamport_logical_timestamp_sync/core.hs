module Lamport
  ( Node
  , Message
  , newNode
  , getTimestamp
  , increment
  , send
  , receive
  ) where

newtype Node = Node Int deriving (Show, Eq)
newtype Message = Message Int deriving (Show, Eq)

-- Create a new node with an initial timestamp
newNode :: Int -> Node
newNode n = Node n

-- Retrieve the current timestamp of a node
getTimestamp :: Node -> Int
getTimestamp (Node t) = t

-- Increment the node's timestamp (local event)
increment :: Node -> Node
increment (Node t) = Node (t + 1)

-- Send a message: node increments and returns the message with the old timestamp
send :: Node -> (Node, Message)
send (Node t) = (Node (t + 1), Message t)

-- Receive a message: update timestamp to max(current, message) + 1
receive :: Node -> Message -> Node
receive (Node t) (Message m) = Node (max t m + 1)
