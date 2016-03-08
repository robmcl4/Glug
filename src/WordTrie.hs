module WordTrie (
    Trie
,   makeTrie
,   makeTrieWithWords
,   addStr
,   containsStr
) where


import Data.List (foldl')


data Tree = Tree Char Trie Tree Tree
          | Empty


insertToTree :: Tree -> Char -> Tree
insertToTree Empty c = Tree c makeTrie Empty Empty
insertToTree (Tree x t left right) c
    | c == x    = Tree x t left right
    | c <  x    = Tree x t (insertToTree left c) right
    | c >  x    = Tree x t left (insertToTree right c)


findFromTree :: Tree -> Char -> Maybe Trie
findFromTree Empty _ = Nothing
findFromTree (Tree x t left right) c
    | c == x    = Just t
    | c <  x    = findFromTree left c
    | c >  x    = findFromTree right c


findAndModify  :: Tree -> Char -> (Maybe Trie -> Trie) -> Tree
findAndModify Empty c f = Tree c (f Nothing) Empty Empty
findAndModify (Tree x t left right) c f
    | c == x    = Tree x (f $ Just t) left right
    | c <  x    = Tree x t (findAndModify left c f) right
    | c >  x    = Tree x t left (findAndModify left c f)


data Trie = Trie Bool Tree


makeTrie :: Trie
makeTrie = Trie False Empty


makeTrieWithWords :: [String] -> Trie
makeTrieWithWords = foldl' (addStr) makeTrie


addStr :: Trie -> String -> Trie
addStr (Trie _ Empty) s     = mkTrieOfString s
addStr (Trie term t) (x:xs) = Trie term (findAndModify t x (\mtri ->
        case mtri of
            Nothing  -> mkTrieOfString xs
            Just tri -> addStr tri xs
    ))


containsStr :: Trie -> String -> Bool
containsStr (Trie term _)  []     = term
containsStr (Trie _ Empty) _      = False
containsStr (Trie term t)  (x:xs) = case subTree of
        Nothing   -> False
        Just t2   -> if null xs then term else (containsStr t2 xs)
    where subTree = findFromTree t x


mkTrieOfString :: String -> Trie
mkTrieOfString []     = makeTrie
mkTrieOfString (x:xs) = Trie (null xs) $ Tree x (mkTrieOfString xs) Empty Empty
