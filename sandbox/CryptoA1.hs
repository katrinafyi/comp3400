module CryptoA1 where

import qualified Data.Map.Strict as M
import           Data.Foldable (Foldable(foldr'))
import           Data.List (sortOn, genericLength, transpose)
import           Data.Ord (Down(Down))
import           Data.Char (ord, chr, toUpper)

lettersByFreq :: [Char]
lettersByFreq = "etaoinshrdlcumwfgypbvkjxqz"

ciphertext :: [Char]
ciphertext =
  "BPMVMFBVMEAQBMUEIAIJWCBIJWZLMZASQZUQAPEQBPBPMXMWXTMAZMXCJTQKWNEITMAVWWVMPCZBRCABINMEAPWBAMFKPIVOMLIKZWAABPMZQDMZEGMVMIZPIGBGXQKITTGZIUJCVKBQWCABPMGWCBPNCTXZMAQLMVBNWZTQNMWEIQVOTGVLEZDQQPILJTIUMLMVOTIVLAQUXMZQITQABGMIZVQVOANWZICVQNQMLJZQBIQVMYCITTGBGXQKITTGXIZTQIUMVBPILVWBAWUCKPIAMDMVUILMIABIBMUMVBIJWCBBPMQVKQLMVBBPMVMEAOZWCVLWVJCBQEIAVBZMITTGXIGQVOIBBMVBQWVIVMENCAQWVXTIVBPILWXMVMLQVLCVOMVMAAIVLBPMXZMAQLMVBPILJMMVBPMZMBWWXMVQBPMOZQVVMLLCBQNCTTGIABPMNTIAPJCTJAEMVBWNNQZMBCZVMLBWUGXIXMZIVLZMILIABWZGIJWCBIXIZTQIUMVBIZGJQTTBWZMUWDMBPMLWLWAXZWBMKBMLAXMKQMAABIBCAINBMZBPMQZABIOOMZQVOQVKZMIAMQVVCUJMZAJCBQKWCTLVBKWVKMVBZIBMBPMKZQUMIPILNQTTMLUGUQVLEQBPQBACVEMTKWUMUMUWZQMAQBEIATCKSGNWZUMBPIBUGXIOMZJTMMXMLIVLJZWCOPBEQBPQBIUCKPVMMLMLZMITQBGKPMKSQBWAAMLINMEVWBMAWVBPMKWCVBMZIVLAXZQVBMLWCBWNBPMLWWZIABPMBWILVMEAIVKPWZEWUIVAWUJMZTGIVVWCVKMLBPIBIGWCVOACZZMITQABPILJMMVSQTTMLABIJJMLBWLMIBPJGIOIVOILPMZQVOBWIZILQKITAKPWWTWNNZMVKPQUXZMAAQWVQABA"

ciphertext2 :: [Char]
ciphertext2 =
  "XYRIKLYEJVCGMCQEWNTZUGDVBZTYVWXACMVVBJQJBXLRHPOCOYFZUJYFYANOSZUYMIHSSAWSUEWDHBQZIAFZZTFHRPPQHKIQSNRBVMGPULMZRJHYRQYNZXUCXLHTCNOFLZQGFYAUEIBXMGPUSSYGZQUYSLFQTYRXSGCLVIYBEQCRKYQYDJUPVQXUHFYSXEQHHTTQHHWVEAEELUYQINCINJYIAJMSWHUYRVXTEQECGIYHIUHSWLLWSCQSUPPLHLRYLUZMDNLEQFZTFRGHKXUWIIWYMNGYNAJUXIZWYTNEIHBJBIMAUTDRWNUMDYAWTJZUAGIAJKNMANKYTEHUYTOJECVFRAGWYABYIREAWHFHVCBVFFERGZLDBSYAMTCRHHFRERVMNVTNLICJPSVGYABXUHFRWYTUHLRBEITIAAEHRJLVVDYHHHLTFHOFLIINLISDLSUEOYJIQIRGGQIEHNHZDYHXBEJPNSYEIDXVINVLSGRLLIRIYXNHLRYLUZMDNEVLTTLYWIEMCIZIGZPDBGIFXHIXIPLOSCHWVMIMXEGMDASWYEBXYMVFLLGTHLVVWCRGEWLSRLHACCVIVFTFTVFIHTTHXGBFNEAWLNBUNLIPAXENKUQNYFPIQEJMVQXJQJBMXFMYWROWBUUGIQBJTEFLNJIIFYGXQQOEPYGPQNQCCSREEEFRMFYHEQTCOHJBGEYNLMGSXUPKHRMTYHVRSWIGBWUMSEMXBKDEQDZREDIXIFGYTUHWBCDNIVNFOSCUCABUXSXBXEHRGIBZQMXLRLZAQQYJAQHGLBJHOZDHFWCVIVYQLNAROAKUXXLNLLYBXHTAKLVINDTSGKUQJUYOMYDPDFWUOJUXXSQWLTUESNOQHKEQZPRVQAGWQLEHVULLFFBBWBIJJEWYCULGCZBPYMTBYJVLETZ"

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
    lettersPaired = M.fromList $ zip lettersSorted lettersByFreq
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


computePairs :: String -> [(Char, Char)]
computePairs (x:xs) = fmap ((,) x) xs ++ computePairs xs
computePairs [] = []

indexOfCoincidence :: Fractional a => String -> a
indexOfCoincidence s = fromIntegral numEqual / fromIntegral numPairs
  where
    pairs = computePairs s
    numPairs = length pairs
    numEqual = length . filter (uncurry (==)) $ pairs

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

minMax :: Ord a => [a] -> (a,a)
minMax xs = (minimum xs, maximum xs)

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

chunkToColumns :: Int -> String -> [String]
chunkToColumns n = transpose . chunks n

tabulate :: (Show a, Show b) => M.Map a b -> [String]
tabulate m = makeRow <$> M.toList m
  where
    makeRow (k, v) = show k ++ "\t" ++ show v

printMap :: (Show a, Show b) => M.Map a b -> IO ()
printMap = mapM_ putStrLn . tabulate

friedmansMethod :: String -> M.Map Int Double
friedmansMethod s = M.fromList $ (\i -> (i, coincidences i)) <$> [2..20]
  where
    len = length s
    coincidences m = average $ indexOfCoincidence <$> chunkToColumns m s
