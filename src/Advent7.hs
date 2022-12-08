{-# LANGUAGE OverloadedStrings #-}

module Advent7(main) where

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Data.Functor
import Util
import Data.List
import Debug.Trace
import Prettyprinter

data CD = Up | Top | Down String deriving (Show)
data LS = LS deriving (Show)
data Dir = Dir String deriving (Show)
data File = File Int String deriving (Show, Eq)
data LSPart = FilePart File | DirPart Dir deriving (Show)
data LSResult = LSResult [LSPart] deriving (Show)
data Entry = CDEntry CD | LSEntry LSResult deriving (Show)


data DirTree = DirTree String (Maybe DirTree) [File] [DirTree]

getName :: DirTree -> String
getName (DirTree name _ _ _) = name

instance Show DirTree where
  show (DirTree name (Just parent) files dirs) = name ++ " p:" ++ (getName parent) ++ " " ++ show files ++ " " ++ show dirs
  show (DirTree name Nothing files dirs) = name ++ " p:None" ++ " " ++ show files ++ " " ++ show dirs

instance Pretty DirTree where
  pretty d@(DirTree name _ files dirs) = pretty name <> (if treeSize d <= 1000000 then (pretty ("*" :: String)) else (pretty ("" :: String))) <> nest 2 (line <> (vsep $ map pretty files ++ map pretty dirs))

instance Pretty File where
  pretty (File size name) = pretty name <> " " <> pretty size


cd :: Parser CD
cd = do
  string "$ cd "
  part <- string ".." <|> string "/" <|> many1 alphaNum
  newline
  case part of
    ".." -> return Up
    "/" -> return Top
    _ -> return (Down part)

ls :: Parser LS
ls = do
  string "$ ls"
  newline
  return LS

dir :: Parser Dir
dir = ((string "dir " >> many1 alphaNum) <&> Dir) <* newline

file :: Parser File
file = do
  size <- many1 digit
  spaces
  name <- manyTill anyChar newline
  return (File (read size) name)

lsPart :: Parser LSPart
lsPart = try (file <&> FilePart) <|> (dir <&> DirPart)

lsCmd :: Parser LSResult
lsCmd = ls >> (LSResult <$> manyTill lsPart (notFollowedBy lsPart))

parseEntry :: Parser Entry
parseEntry = try (CDEntry <$> cd) <|> (LSEntry <$> lsCmd)

parseAll :: Parser [Entry]
parseAll = manyTill parseEntry eof


sameDir :: DirTree -> DirTree -> Bool
sameDir (DirTree name _ _ _) (DirTree name' _ _ _) = name == name'

setParent :: DirTree -> DirTree -> DirTree
setParent p (DirTree name _ files dirs) = DirTree name (Just p) files dirs

setChild :: DirTree -> DirTree -> DirTree
setChild c (DirTree name parent files dirs) = DirTree name parent files (nubBy sameDir (c:dirs))

modify :: (DirTree -> DirTree) -> DirTree -> DirTree
modify f dt = knot where
  transformed@(DirTree newName newParent newFiles newDirs) = f dt
  finalParent = case newParent of
    Just p -> Just (modify (setChild transformed) p)
    Nothing -> Nothing
  finalChildren = map (modify (setParent transformed)) newDirs
  knot = DirTree newName finalParent newFiles finalChildren


addDir :: DirTree -> DirTree -> DirTree
addDir curr@(DirTree name parent files dirs) child = DirTree name parent files (nubBy sameDir (child:dirs))

addFile :: DirTree -> File -> DirTree
addFile (DirTree name parent files dirs) newFile = DirTree name parent (newFile : files) dirs

addPart' :: DirTree -> LSPart -> DirTree
addPart' dirTree (FilePart file) = addFile dirTree file
addPart' dirTree (DirPart (Dir name)) = addDir dirTree (DirTree name (Just dirTree) [] [])

addPart :: DirTree -> LSPart -> DirTree
addPart dirTree part = modify (`addPart'` part) dirTree

toRoot :: DirTree -> DirTree
toRoot dt@(DirTree _ Nothing _ _) = dt
toRoot (DirTree _ (Just parent) _ _) = toRoot parent

getChild :: DirTree -> String -> DirTree
getChild (DirTree _ _ _ dirs) name =  x where
  result = find (\(DirTree name' _ _ _) -> name == name') dirs
  x = case result of
    Nothing -> error ("Could not find child " ++ name)
    Just d -> d

reduceTree :: DirTree -> Entry -> DirTree
reduceTree (DirTree _ (Just parent) _ _) (CDEntry Up) = parent
reduceTree (DirTree _ Nothing _ _) (CDEntry Up) = error "Cannot go up from root"
reduceTree dt (CDEntry (Down desc)) =  getChild dt desc
reduceTree dt (CDEntry Top) =  toRoot dt
reduceTree dt (LSEntry (LSResult parts)) =  foldl addPart dt parts

fileSize :: File -> Int
fileSize (File size _) = size

treeSize :: DirTree -> Int
treeSize (DirTree _ _ files dirs) = sum (map fileSize files) + sum (map treeSize dirs)

treeFilter :: (DirTree -> Bool) -> DirTree -> [DirTree]
treeFilter f dt@(DirTree _ _ _ dirs) = if f dt then dt:concatMap (treeFilter f) dirs else concatMap (treeFilter f) dirs


main :: IO ()
main = do
  s <- resource "terminal-session"
  let entries = runParser parseAll () "ls" s
  print entries
  let entryResult = case entries of
        Left err -> error (show err)
        Right result -> result
  let tree = toRoot $ foldl reduceTree (DirTree "/" Nothing [] []) entryResult
  print (tree)
  print (treeSize tree)
  let smallEnoughTrees = fmap (\t -> (treeSize t, t)) $ treeFilter (\dt -> treeSize dt <= 100000) tree
  mapM_ print smallEnoughTrees
  print (length (treeFilter (\(DirTree _ _ fs _) -> nub fs /= fs) tree))
  print $ sum $ fmap fst smallEnoughTrees

  let unused = 70000000 - (treeSize tree)
  let neededSpace = 30000000 - unused

  print $ minimum $ fmap treeSize $ treeFilter (\dt -> treeSize dt >= neededSpace) tree
