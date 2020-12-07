# HS Runeterra

Haskell port of the [**RiotGames/LoRDeckCodes**](https://github.com/RiotGames/LoRDeckCodes/) library, made for encoding and decoding [**Legends of Runeterra**](http://playruneterra.com) card decks. Below is an example code for the [Warmother Control](https://lor.mobalytics.gg/decks/bmibq02t9lr96dua7ta0) deck by **prohibit_hb** on Mobalytics.

```
CEBAIAIFAEHSQNQIAEAQGDAUDAQSOKJUAIAQCBI5AEAQCFYA
```

## Install
```
stack install runeterra
```

## Cards, decks and how to encode them
If you want to understand the process of the encoding and decoding decks, please refer to the [original library](https://github.com/RiotGames/LoRDeckCodes/#cards--decks), as Riot has done a great job explaining it there.

## Usage
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Runeterra.Deck
import Runeterra.Decode
import Runeterra.Encode

-- Decoding
deck :: Deck
deck = encodeDeck "CEAAECABAQJRWHBIFU2DOOYIAEBAMCIMCINCILJZAICACBANE4VCYBABAILR2HRL"
>> Deck {
    deckFormat = 1, 
    deckVersion = 1, 
    cards = [
      Card {set = Launch Set, faction = PZ, code = 19, count = 2}, 
      Card {set = Launch Set, faction = PZ, code = 27, count = 2},
      Card {set = Launch Set, faction = PZ, code = 28, count = 2},
      Card {set = Launch Set, faction = PZ, code = 40, count = 2},
      Card {set = Launch Set, faction = PZ, code = 45, count = 2},
      Card {set = Launch Set, faction = PZ, code = 52, count = 2},
      Card {set = Launch Set, faction = PZ, code = 55, count = 2},
      Card {set = Launch Set, faction = PZ, code = 59, count = 2},
      Card {set = Launch Set, faction = IO, code = 6, count = 2},
      Card {set = Launch Set, faction = IO, code = 9, count = 2},
      Card {set = Launch Set, faction = IO, code = 12, count = 2},
      Card {set = Launch Set, faction = IO, code = 18, count = 2},
      Card {set = Launch Set, faction = IO, code = 26, count = 2},
      Card {set = Launch Set, faction = IO, code = 36, count = 2},
      Card {set = Launch Set, faction = IO, code = 45, count = 2},
      Card {set = Launch Set, faction = IO, code = 57, count = 2},
      Card {set = Launch Set, faction = PZ, code = 13, count = 1},
      Card {set = Launch Set, faction = PZ, code = 39, count = 1},
      Card {set = Launch Set, faction = PZ, code = 42, count = 1},
      Card {set = Launch Set, faction = PZ, code = 44, count = 1},
      Card {set = Launch Set, faction = IO, code = 23, count = 1},
      Card {set = Launch Set, faction = IO, code = 29, count = 1},
      Card {set = Launch Set, faction = IO, code = 30, count = 1},
      Card {set = Launch Set, faction = IO, code = 43, count = 1}
    ]
  }

-- Encoding
code :: ByteString
code = encodeDeck deck
>> "CEAAECABAQJRWHBIFU2DOOYIAEBAMCIMCINCILJZAICACBANEVCYBABAILR2HRL"
```