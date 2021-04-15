module House (rhyme) where

import           Data.List (intercalate, tails)
import           Control.Arrow ((***), (<<<))

data RhymeItem = RhymeItem { rhymeVerb :: String, rhymeThing :: String }
  deriving (Eq, Show)

rhymeItems :: [RhymeItem]
rhymeItems =
  [ RhymeItem "" "the horse and the hound and the horn"
  , RhymeItem "belonged to" "the farmer sowing his corn"
  , RhymeItem "kept" "the rooster that crowed in the morn"
  , RhymeItem "woke" "the priest all shaven and shorn"
  , RhymeItem "married" "the man all tattered and torn"
  , RhymeItem "kissed" "the maiden all forlorn"
  , RhymeItem "milked" "the cow with the crumpled horn"
  , RhymeItem "tossed" "the dog"
  , RhymeItem "worried" "the cat"
  , RhymeItem "killed" "the rat"
  , RhymeItem "ate" "the malt"
  , RhymeItem "lay in" "the house that Jack built."]

rhymeThisLine :: RhymeItem -> String
rhymeThisLine = ("This is " ++) . rhymeThing

rhymeThatLine :: RhymeItem -> String
rhymeThatLine = ("that " ++) . unwords . sequence [rhymeVerb, rhymeThing]

rhymeSection :: [RhymeItem] -> [String]
rhymeSection =
  uncurry (++) <<< fmap rhymeThisLine *** fmap rhymeThatLine <<< splitAt 1

makeRhyme :: [RhymeItem] -> String
makeRhyme = unlines
  . intercalate [""]
  . fmap rhymeSection
  . dropWhile null
  . reverse
  . tails

rhyme :: String
rhyme = makeRhyme rhymeItems
