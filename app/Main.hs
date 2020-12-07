{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (forM_)
import qualified Data.ByteString as B
import           Runeterra.Decode
import           Runeterra.Encode
import           Runeterra.Deck
import           Text.Printf (printf)

sample :: B.ByteString
sample = "CEAAECABAQJRWHBIFU2DOOYIAEBAMCIMCINCILJZAICACBANE4VCYBABAILR2HRL"

printDeck :: Deck -> IO ()
printDeck (Deck fmt ver cards) = do
  printf "Version %d.%d \n" fmt ver
  forM_ cards $ \card -> putStr "  " >> print card

main :: IO ()
main = do
  putStrLn "Encoded deck:"
  print sample
  let deck = decodeDeck sample
  putStrLn "Decoded deck:"
  printDeck deck
  let code = encodeDeck deck
  putStrLn "Rencoded deck:"
  print code
  putStrLn "Redecoded deck:"
  printDeck $ decodeDeck code
  putStrLn "Summary"
  print sample
  putStrLn "vs."
  print code
  putStrLn $ "= " ++ show (sample == code)