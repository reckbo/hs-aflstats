module AFLTables.Types where

type EventID = String

type Team = String

type Player = String

data Alignment = Home | Away
  deriving (Show,Eq)