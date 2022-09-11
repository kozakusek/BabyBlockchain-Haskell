-- bk418339
module HashTree where
import Hashable32 ( Hashable(hash), Hash, showHash )
import Data.Bifoldable (Bifoldable(bifoldr))
import Data.Maybe (fromJust, fromMaybe, isJust)

type MerklePath = [Either Hash Hash]

data Tree a = Node Hash (Tree a) (Maybe (Tree a)) | Leaf Hash a
data MerkleProof a = MerkleProof a MerklePath

instance (Show a) => (Show (MerkleProof a)) where
  showsPrec d (MerkleProof v p) = showParen (d /= 0) $
    ("MerkleProof " ++) . showsPrec 11 v . (" " ++) . (showMerklePath p ++)

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

twig :: Hashable a => Tree a -> Tree a
twig a = let h = treeHash a in Node (hash (h, h)) a Nothing

node :: Hashable a => Tree a -> Tree a -> Tree a
node a b = Node (hash (treeHash a, treeHash b)) a (Just b)

buildTree :: Hashable a => [a] -> Tree a
buildTree = head . until (null . tail) go . map leaf . validate
  where
    go = reverse . foldl2 (\a e1 e2 -> node e1 e2:a) (\a e -> twig e:a) []
    validate l = if null l then error "buildTree: Cannot create an empty Tree" else l
    foldl2 f1 f2 a []        = a
    foldl2 f1 f2 a [h]       = f2 a h
    foldl2 f1 f2 a (h1:h2:t) = foldl2 f1 f2 (f1 a h1 h2) t

treeHash :: Tree a -> Hash
treeHash (Node h _ _) = h
treeHash (Leaf h _)   = h

drawTree :: Show a => Tree a -> String
drawTree t = f t "" ""
  where
    f (Leaf h a)   pref = (pref ++) . (showHash h ++) . (" " ++) . (show a ++) . ("\n" ++)
    f (Node h l r) pref = (pref ++) . (showHash h ++) . case r of
      Nothing -> (" +\n" ++) . f l (pref ++ "  ")
      Just v  -> (" -\n" ++) . f l (pref ++ "  ") . f v (pref ++ "  ")

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths a (Leaf h _) = [[] | hash a == h]
merklePaths a (Node h l r) = case r of
  Just r  -> ((Left (treeHash r):) <$> merklePaths a l) ++ ((Right (treeHash l):) <$> merklePaths a r)
  Nothing -> (Left (treeHash l):) <$> merklePaths a l

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof a (Leaf h _) = if hash a == h then Just $ MerkleProof a [] else Nothing
buildProof a (Node h l r)
  | isJust l' = Just $ MerkleProof a $ Left (treeHash $ fromMaybe l r):path (fromJust l')
  | isJust r' = Just $ MerkleProof a $ Right (treeHash l):path (fromJust r')
  | otherwise = Nothing
  where
    l' = buildProof a l
    r' = r >>= buildProof a
    path (MerkleProof _ p) = p

showMerklePath :: MerklePath -> String
showMerklePath = concatMap $ bifoldr (\x a -> "<" ++ showHash x ++ a) (\x a -> ">" ++ showHash x ++ a) []

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a p) = h == foldr f (hash a) p
  where f e a = either (\x -> hash (a, x)) (\x -> hash (x, a)) e
