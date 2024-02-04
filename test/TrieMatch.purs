module Test.TrieMatch where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Trie (Trie, children, isLeaf, lookup)
import Data.Tuple.Nested (type (/\), (/\))

data Case var a = Const a | WildCard | Variable var
type Pattern var a = List (Case var a)

-- | Match a pattern against a trie, returning all possible matches.
match :: forall var a. Hashable var => Hashable a => Pattern var a -> Trie a -> HashSet (HashMap var a /\ List a)
match pattern trie = matchWithVariables HashMap.empty pattern trie

matchWithVariables :: forall var a. Hashable var => Hashable a => HashMap var a -> Pattern var a -> Trie a -> HashSet (HashMap var a /\ List a)
matchWithVariables variables pattern trie = go pattern trie variables Nil # HashSet.map (\(vars /\ path) -> vars /\ reverse path)
  where
  go :: Pattern var a -> Trie a -> HashMap var a -> List a -> HashSet (HashMap var a /\ List a)
  go pat tr vars path = case pat of
    Nil -> if isLeaf tr then HashSet.singleton (vars /\ path) else HashSet.empty
    Const a : xs -> goConst a xs
    WildCard : xs -> HashSet.unions $ HashMap.toArrayBy (\a tr' -> go xs tr' vars (a : path)) $ children tr
    Variable v : xs -> case HashMap.lookup v vars of
      Just a -> goConst a xs
      Nothing -> HashSet.unions $ HashMap.toArrayBy (\a tr' -> go xs tr' (HashMap.insert v a vars) (a : path)) $ children tr
    where
    goConst a xs = case lookup a tr of
      Just tr' -> go xs tr' vars (a : path)
      Nothing -> HashSet.empty

-- | Match a patterns against a trie, returning all possible matches.
matchMany :: forall var a. Hashable var => Hashable a => List (Pattern var a) -> Trie a -> HashSet (HashMap var a /\ List (List a))
matchMany patterns trie = go patterns HashMap.empty
  where
  go :: List (Pattern var a) -> HashMap var a -> HashSet (HashMap var a /\ List (List a))
  go pats vars = case pats of
    Nil -> HashSet.singleton (vars /\ Nil)
    pat : pats' ->
      HashSet.unions
        $ HashSet.toArray
        $ HashSet.map (\(vars' /\ result) -> HashSet.map (map (result : _)) $ go pats' vars')
        $ matchWithVariables vars pat trie
