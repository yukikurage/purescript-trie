module Test.Main where

import Prelude

import Data.Array (fromFoldable, toUnfoldable)
import Data.HashMap (HashMap)
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.List (List(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, takeWhile, toCharArray)
import Data.Trie as Trie
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Node.FS.Stream (createReadStream)
import Node.Process (stdin)
import Node.Stream.Aff (readSome, readableToStringUtf8, toStringUTF8)
import Test.TrieMatch (Case(..), Pattern, match, matchMany)

toCharList :: String -> List Char
toCharList = toUnfoldable <<< toCharArray

toPattern :: String -> Pattern Char Char
toPattern = map f <<< toCharList
  where
  f :: Char -> Case Char Char
  f c = case c of
    '_' -> WildCard
    x | isVar x -> Variable x
    x -> Const x

  isVar :: Char -> Boolean
  isVar c = '0' <= c && c <= '9'

main :: Effect Unit
main = do
  launchAff_ testBig

inputPatterns :: Aff (List (Pattern Char Char))
inputPatterns = go Nil
  where
  go acc = do
    { buffers } <- readSome stdin
    str <- toStringUTF8 buffers
    case str of
      "\n" -> do
        pure acc
      _ -> go $ Cons (toPattern $ takeWhile (_ /= '\n') str) acc

testBig :: Aff Unit
testBig = do
  log "START"
  log "Read file"
  rs <- liftEffect $ createReadStream "./ref/VDLJ_Ver1_0_General-Learners.txt"
  str <- readableToStringUtf8 rs
  log "Make trie"
  let
    trie = Trie.fromFoldable $ map toCharList $ String.split (String.Pattern "\n") str
  log "Please input patterns:"
  patterns <- inputPatterns
  log "Match"
  let
    -- patterns = toUnfoldable $ map toPattern
    --   [ "12"
    --   , "31__2"
    --   , "1ン3"
    --   ]

    prettyMatchResult :: HashSet (HashMap Char Char /\ List (List Char)) -> Array (Array String)
    prettyMatchResult = map f <<< HashSet.toArray
      where
      f :: HashMap Char Char /\ List (List Char) -> Array String
      f (_ /\ p) = map (fromCharArray <<< fromFoldable) $ fromFoldable p
  logShow $ prettyMatchResult $ matchMany patterns trie

test :: Effect Unit
test = do
  let
    trie = Trie.fromFoldable $ map toCharList [ "hello", "world", "hello world", "holla", "hollo" ]
  logShow $ Trie.member (toCharList "hello") trie
  logShow $ Trie.member (toCharList "world") trie
  logShow $ Trie.member (toCharList "hello world") trie
  logShow $ Trie.member (toCharList "hello world!") trie

  let
    prettyMatchResult :: HashSet (HashMap Char Char /\ List Char) -> Array String
    prettyMatchResult = map f <<< HashSet.toArray
      where
      f :: HashMap Char Char /\ List Char -> String
      f (_ /\ p) = fromCharArray $ fromFoldable p
  logShow $ prettyMatchResult $ match (toPattern "hello") trie
  logShow $ prettyMatchResult $ match (toPattern "hell_") trie
  logShow $ prettyMatchResult $ match (toPattern "h_ll_") trie
  logShow $ prettyMatchResult $ match (toPattern "h1ll1") trie
