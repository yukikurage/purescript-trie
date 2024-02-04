module Data.Trie where

import Prelude

import Data.Foldable (class Foldable)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable)
import Data.List (List(..), foldr)
import Data.Maybe (Maybe(..))

data Trie a = Trie Boolean (HashMap a (Trie a))

derive instance Eq a => Eq (Trie a)

-- | Check if a trie is a leaf.
isLeaf :: forall a. Trie a -> Boolean
isLeaf (Trie b _) = b

-- | Get the children of a trie.
children :: forall a. Trie a -> HashMap a (Trie a)
children (Trie _ m) = m

-- | Create an empty trie.
empty :: forall a. Trie a
empty = Trie false HashMap.empty

-- | Create a trie with a single value.
singleton :: forall a. Hashable a => List a -> Trie a
singleton as = foldr (\a tr -> Trie false (HashMap.singleton a tr)) (Trie true HashMap.empty) as

-- | Lookup a value in the trie.
lookup :: forall a. Hashable a => a -> Trie a -> Maybe (Trie a)
lookup a (Trie _ m) = HashMap.lookup a m

-- | Insert a value into the trie.
insert :: forall a. Hashable a => List a -> Trie a -> Trie a
insert as tr = case as of
  Nil -> Trie true (children tr)
  Cons a as' -> case lookup a tr of
    Just tr' -> Trie (isLeaf tr) (HashMap.insert a (insert as' tr') (children tr))
    Nothing -> Trie (isLeaf tr) (HashMap.insert a (insert as' empty) (children tr))

-- | Check if a value is in the trie.
member :: forall a. Hashable a => List a -> Trie a -> Boolean
member as tr = case as of
  Nil -> isLeaf tr
  Cons a as' -> case lookup a tr of
    Just tr' -> member as' tr'
    Nothing -> false

-- | Delete a value from the trie.
delete :: forall a. Hashable a => List a -> Trie a -> Trie a
delete as tr = case as of
  Nil -> Trie false (children tr)
  Cons a as' -> case lookup a tr of
    Just tr' -> Trie (isLeaf tr) (HashMap.insert a (delete as' tr') (children tr))
    Nothing -> tr

prefix :: forall a. Hashable a => a -> Trie a -> Trie a
prefix a (Trie b m) = Trie false (HashMap.singleton a (Trie b m))

fromFoldable :: forall a f. Foldable f => Hashable a => f (List a) -> Trie a
fromFoldable = foldr insert empty
