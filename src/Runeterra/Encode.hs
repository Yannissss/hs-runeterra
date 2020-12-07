module Runeterra.Encode (encodeDeck) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString.Base32
import           Data.Packer
import           Data.Word
import           Runeterra.Deck
import           Control.Arrow (first, second)

-- | Encoding ---
segregate :: (a -> Bool) -> [a] -> ([a], [a])
segregate _ [] = ([], [])
segregate f (x:xs)
  | f x = first (x:) $ segregate f xs
  | otherwise = second (x:) $ segregate f xs

segregates :: [a -> Bool] -> [a] -> [[a]]
segregates [] xs = [xs]
segregates (f:fs) xs = us:segregates fs vs
  where
    (us, vs) = segregate f xs

sortCardsCount :: [Card] -> ([Card], [Card], [Card])
sortCardsCount cards = (cards1, cards2, cards3)
  where
    countFn n = (== n) . count

    cards1:cards2:cards3:_ = segregates (map countFn [1, 2, 3]) cards

sortCardsSet :: [Card] -> [[Card]]
sortCardsSet = segregates $ map (\st -> (== st) . set) [Set1, Set2, Set3]

sortCardsFaction :: [Card] -> [[Card]]
sortCardsFaction = segregates
  $ map (\fac -> (== fac) . faction)
  $ reverse [DE, FR, IO, NX, PZ, SI, BW, MT]

putVarInt :: Integer -> Packing ()
putVarInt n = do
  let val = fromInteger $ n .&. 0x7F
      n' = shift n (-7)
  if n' == 0
    then putWord8 val
    else do
      let val' = 0x80 .|. val
      putWord8 val'
      putVarInt n'

putCardBlock :: Set -> Faction -> [Card] -> Packing ()
putCardBlock set faction cards = do
  unless (null cards)
    $ do
      putVarInt $ toInteger $ length cards
      putVarInt $ getSetCode set
      putVarInt $ getFactionID faction
      forM_ cards $ putVarInt . toInteger . code

encodeDeck :: Deck -> B.ByteString
encodeDeck (Deck fmt ver cards) = encodeBase32' (runPacking 8192 encoder)
  where
    encoder = do
      let fmtver =
            fromInteger $ toInteger $ shift (fmt .&. 0xF) 4 .|. (ver .&. 0xF)
      let (cards1, cards2, cards3) = sortCardsCount cards
      putWord8 fmtver
      forM_ [cards3, cards2, cards1]
        $ \cards -> do
          let cards' = sortCardsSet cards
              cards'' = concatMap sortCardsFaction cards'
              cards''' = filter (not . null) cards''
          putVarInt $ toInteger $ length cards'''
          forM_ cards'''
            $ \cardBlock -> do
              if null cardBlock
                then return ()
                else do
                  let sample = head cardBlock
                  putCardBlock (set sample) (faction sample) cardBlock