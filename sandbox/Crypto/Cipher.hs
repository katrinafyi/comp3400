{-# LANGUAGE DefaultSignatures #-}

module Cipher where

import qualified Data.List.NonEmpty as N
import           Data.Maybe (fromMaybe)
import           Data.Char (ord, chr, toUpper, toLower)
import           Data.Coerce (coerce)

infixl 7 %

(%) :: Integral a => a -> a -> a
(%) = mod

modInvs :: Integral a => a -> a -> [a]
n `modInvs` d = filter (\x -> x * n `mod` d == 1) [0 .. d]

getSingleton :: [a] -> Maybe a
getSingleton [x] = Just x
getSingleton _ = Nothing

modInv :: (Show a, Integral a) => a -> a -> a
n `modInv` d = fromMaybe
  (error $ show n ++ " has multiple inverses modulo " ++ show d)
  $ getSingleton
  $ N.toList
  $ fromMaybe (error $ show n ++ " has no inverses modulo " ++ show d)
  $ N.nonEmpty
  $ modInvs n d

infixl 8 \%

(\%) :: (Show a, Integral a) => a -> a -> a
(\%) = modInv

newtype CipherLetter a = CipherLetter { getCipherLetter :: Int }
  deriving (Eq)

instance Show (CipherLetter a) where
  showsPrec p (CipherLetter x) = showString "CipherLetter " . showsPrec 11 x

  showList xs = showString "cipherText "
    . showList (fmap (toUpper . toLetter . getCipherLetter) xs)

newtype PlainLetter = PlainLetter { getPlainLetter :: Int }
  deriving (Eq)

instance Show PlainLetter where
  showsPrec p (PlainLetter x) = showString "CipherLetter " . showsPrec 11 x

  showList xs = showString "plainText "
    . showList (fmap (toLower . toLetter . getPlainLetter) xs)

cipherLetter :: Int -> CipherLetter a
cipherLetter = CipherLetter . (`mod` 26)

plainLetter :: Int -> PlainLetter
plainLetter = PlainLetter . (`mod` 26)

plainText :: String -> PlainText
plainText = fmap (plainLetter . subtract (ord 'A') . ord . toUpper)

cipherText :: String -> CipherText a
cipherText = fmap (cipherLetter . getPlainLetter) . plainText

toLetter :: Int -> Char
toLetter = chr . (+ ord 'A')

type CipherText a = [CipherLetter a]

type PlainText = [PlainLetter]

class Cipher a where
  encode :: a -> PlainText -> CipherText a
  decode :: a -> CipherText a -> PlainText

  default encode :: Monoalphabetic a => a -> PlainText -> CipherText a
  encode c = fmap (monoEncode c . CipherLetter . getPlainLetter)

  default decode :: Monoalphabetic a => a -> CipherText a -> PlainText
  decode c = fmap (PlainLetter . getCipherLetter . monoDecode c)

class Cipher a => Monoalphabetic a where
  monoEncode :: a -> CipherLetter b -> CipherLetter (b, a)
  monoDecode :: a -> CipherLetter (b, a) -> CipherLetter b

class Cipher a => Polyalphabetic a where
  polyEncode :: a -> PlainText -> CipherText a
  polyDecode :: a -> CipherText a -> PlainText

data IdentityCipher = IdentityCipher deriving (Show)
instance Cipher IdentityCipher where
  encode _ = coerce id
  decode _ = coerce id

data AffineCipher = AffineCipher { affineMult :: Int, affineShift :: Int }
  deriving (Show)

instance Cipher AffineCipher

instance Monoalphabetic AffineCipher where
  monoEncode (AffineCipher a b) (CipherLetter x) = cipherLetter $ a * x + b

  monoDecode (AffineCipher a b) (CipherLetter y) = CipherLetter $ a \% 26 * (y - b)

data ViginereKey

data Vigenere = Vigenere { vigenereShifts :: CipherText ViginereKey }
  deriving (Show)

vigenereToAffines :: Vigenere -> [AffineCipher]
vigenereToAffines =
  cycle . fmap (AffineCipher 1 . getCipherLetter) . vigenereShifts

instance Cipher Vigenere where
  encode = polyEncode

  decode = polyDecode

instance Polyalphabetic Vigenere where
  polyEncode vig xs = coerce <$> zipWith monoEncode (vigenereToAffines vig) xs

  polyDecode vig ys = zipWith monoDecode (vigenereToAffines vig)
    $ coerce <$> ys
