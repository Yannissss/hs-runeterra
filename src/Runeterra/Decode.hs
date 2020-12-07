module Runeterra.Decode (decodeDeck) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString.Base32
import           Data.Packer
import           Data.Word
import           Runeterra.Deck

-- | Decoding ---
getVarInt :: Unpacking Integer
getVarInt = aux 0 0
  where
    aux acc step = do
      byte <- getWord8
      let msb = byte .&. 0x80
          val = shift (toInteger $ byte .&. 0x7f) step
      if msb /= 0
        then aux val (step + 7)
        else return val

getCardBlock :: Int -> Unpacking [Card]
getCardBlock n = do
  nbFactions <- getVarInt
  factions <- replicateM (fromInteger nbFactions)
    $ do
      nbCards <- getVarInt
      setCode <- getVarInt
      faction <- getVarInt
      cards <- replicateM (fromInteger nbCards) getVarInt
      let aux card = Card
            (fromSetCode setCode)
            (fromFactionID faction)
            (fromInteger card)
            n
      return $ fmap aux cards
  return $ concat factions

decodeDeck :: B.ByteString -> Deck
decodeDeck deckCode = case decodeBase32 deckCode of
  Left err        -> error "Base32 error"
  Right deckCode' -> runUnpacking decoder deckCode'
  where
    decoder = do
      fmtver <- getWord8
      let format = fromInteger . toInteger $ shift fmtver (-4) .&. 0x0F
          version = fromInteger . toInteger $ fmtver .&. 0x0F
      deck3cs <- getCardBlock 3
      deck2cs <- getCardBlock 2
      deck1cs <- getCardBlock 1
      return $ Deck format version (deck3cs ++ deck2cs ++ deck1cs)
