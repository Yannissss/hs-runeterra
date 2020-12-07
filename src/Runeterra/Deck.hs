module Runeterra.Deck where

data Faction = DE
             | FR
             | IO
             | NX
             | PZ
             | SI
             | BW
             | MT
  deriving (Eq, Ord, Show)

fromFactionID :: (Eq a, Show a, Num a) => a -> Faction
fromFactionID 0 = DE
fromFactionID 1 = FR
fromFactionID 2 = IO
fromFactionID 3 = NX
fromFactionID 4 = PZ
fromFactionID 5 = SI
fromFactionID 6 = BW
fromFactionID 9 = MT
fromFactionID x = error $ "Runeterra.Deck: Unknown faction id, " ++ show x

factionName :: Faction -> String
factionName DE = "Demacia"
factionName FR = "Freljord"
factionName IO = "Ionia"
factionName NX = "Noxus"
factionName PZ = "Piltover & Zaun"
factionName SI = "Shadow Isles"
factionName BW = "Bilgewater"
factionName MT = "Mount Targon"

getFactionID :: Faction -> Integer
getFactionID DE = 0
getFactionID FR = 1
getFactionID IO = 2
getFactionID NX = 3
getFactionID PZ = 4
getFactionID SI = 5
getFactionID BW = 6
getFactionID MT = 9

data Set = Set1
         | Set2
         | Set3
  deriving (Eq, Ord)

instance Show Set where
  show Set1 = "Launch Set"
  show Set2 = "Rising Tides"
  show Set3 = "Call of the Mountain"

fromSetCode :: (Eq a, Show a, Num a) => a -> Set
fromSetCode 1 = Set1
fromSetCode 2 = Set2
fromSetCode 3 = Set3
fromSetCode x = error $ "Runeterra.Deck: Unknown set id, " ++ show x

getSetCode :: Set -> Integer
getSetCode Set1 = 1
getSetCode Set2 = 2
getSetCode Set3 = 3

data Card = Card { set :: Set, faction :: Faction, code :: Int, count :: Int }
  deriving (Eq, Ord, Show)

data Deck = Deck { deckFormat :: Int, deckVersion :: Int, cards :: [Card] }
  deriving (Eq, Ord, Show)
