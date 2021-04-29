module CryptoA1 where

import qualified Data.Map.Strict as M
import           Data.Foldable (Foldable(foldr'))
import           Data.List (sortOn)
import Data.Ord (Down(Down))
import Data.Char (ord, chr, toUpper)

lettersByFreq :: [Char]
lettersByFreq = "etaoinshrdlcumwfgypbvkjxqz"

lettersByFreq2 :: String
lettersByFreq2 = "esiarntolcdugpmhbyfvkwzxjq"

ciphertext :: [Char]
ciphertext =
  "BPMVMFBVMEAQBMUEIAIJWCBIJWZLMZASQZUQAPEQBPBPMXMWXTMAZMXCJTQKWNEITMAVWWVMPCZBRCABINMEAPWBAMFKPIVOMLIKZWAABPMZQDMZEGMVMIZPIGBGXQKITTGZIUJCVKBQWCABPMGWCBPNCTXZMAQLMVBNWZTQNMWEIQVOTGVLEZDQQPILJTIUMLMVOTIVLAQUXMZQITQABGMIZVQVOANWZICVQNQMLJZQBIQVMYCITTGBGXQKITTGXIZTQIUMVBPILVWBAWUCKPIAMDMVUILMIABIBMUMVBIJWCBBPMQVKQLMVBBPMVMEAOZWCVLWVJCBQEIAVBZMITTGXIGQVOIBBMVBQWVIVMENCAQWVXTIVBPILWXMVMLQVLCVOMVMAAIVLBPMXZMAQLMVBPILJMMVBPMZMBWWXMVQBPMOZQVVMLLCBQNCTTGIABPMNTIAPJCTJAEMVBWNNQZMBCZVMLBWUGXIXMZIVLZMILIABWZGIJWCBIXIZTQIUMVBIZGJQTTBWZMUWDMBPMLWLWAXZWBMKBMLAXMKQMAABIBCAINBMZBPMQZABIOOMZQVOQVKZMIAMQVVCUJMZAJCBQKWCTLVBKWVKMVBZIBMBPMKZQUMIPILNQTTMLUGUQVLEQBPQBACVEMTKWUMUMUWZQMAQBEIATCKSGNWZUMBPIBUGXIOMZJTMMXMLIVLJZWCOPBEQBPQBIUCKPVMMLMLZMITQBGKPMKSQBWAAMLINMEVWBMAWVBPMKWCVBMZIVLAXZQVBMLWCBWNBPMLWWZIABPMBWILVMEAIVKPWZEWUIVAWUJMZTGIVVWCVKMLBPIBIGWCVOACZZMITQABPILJMMVSQTTMLABIJJMLBWLMIBPJGIOIVOILPMZQVOBWIZILQKITAKPWWTWNNZMVKPQUXZMAAQWVQABA"

computeFreqs :: String -> M.Map Char Int
computeFreqs = foldr' go M.empty
  where
    go :: Char -> M.Map Char Int -> M.Map Char Int
    go x = M.insertWith (+) x 1

decryptAffine :: String -> String
decryptAffine xs = decodeLetter <$> xs
  where
    freqs = computeFreqs xs
    lettersSorted = fmap fst . sortOn (Down . snd) . M.toList $ freqs
    lettersPaired = M.fromList $ zip lettersSorted lettersByFreq2
    decodeLetter = (lettersPaired M.!)

charToZ26 :: Char -> Int
charToZ26 = subtract (ord 'A') . ord . toUpper

z26ToChar :: Int -> Char
z26ToChar = chr . (+ ord 'a') . (`mod` 26)

decryptShift :: String -> String
decryptShift xs = decodeLetter <$> xs
  where
    freqs = computeFreqs xs
    hopefullyE = fst . head . sortOn (Down . snd) . M.toList $ freqs
    b = ord hopefullyE - ord 'E'
    decodeLetter = z26ToChar . subtract b . charToZ26
