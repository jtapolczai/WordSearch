{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module WordSearch where

import Control.Arrow (second)
import qualified Data.Map as M
import qualified Data.Trie as T
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.IntMultiSet as IMS
import qualified Data.IntSet as IS
import Control.Monad.Loops
import Control.Lens
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe
import Search

import Debug.Trace

type FieldWidth = Int
type WordLength = Int
type Solution = [Int]

type LetterGraph = M.Map Int (Char, [Int])
type WordList = T.Trie ()
data Stop = Stop | Continue deriving (Eq, Show, Ord, Enum, Bounded, Read)

data OneWordNode = OneWordNode {
   _oneWordNodeSolution :: Solution,
   _oneWordNodeStop :: Stop
}
   deriving (Eq, Show)

data MultiWordNode = MultiWordNode {
   _multiWordNodeGraph :: LetterGraph,
   _multiWordNodeSolutions :: [(Solution, LetterGraph)],
   _multiWordNodeRemainingWords :: IMS.IntMultiSet
}
   deriving (Eq, Show)

makeFields ''OneWordNode
makeFields ''MultiWordNode

mkLetterGraph :: FieldWidth -> [Char] -> LetterGraph
mkLetterGraph width cs = M.mapWithKey mkConns initMap
   where
      initMap :: LetterGraph
      initMap = M.fromList $ snd $ foldl' (\acc c -> if c == ' ' then skipLetter acc else addLetter acc c) (0,[]) cs

      addLetter (i,xs) c = (i+1,(i,(c,[])):xs)
      skipLetter (i,xs) = (i+1,xs)

      mkConns :: Int -> (Char, [Int]) -> (Char, [Int])
      mkConns k = second $ const $ filter (flip M.member initMap) $ line width (-1) k ++ line width 0 k ++ line width 1 k

line :: FieldWidth -> Int -> Int -> [Int]
line width offset k = map (+ (width * offset)) $
   (if k `rem` width == 0   then [] else [k-1]) ++
   (if offset == 0          then [] else [k]) ++
   (if (k+1) `rem` width == 0 then [] else [k+1])

readWordList :: FilePath -> IO WordList
readWordList fp = T.fromList . map ((,()) . mkBS) . lines <$> readFile fp

mkWord :: LetterGraph -> Solution -> B.ByteString
mkWord lg = LB.toStrict . BB.toLazyByteString . BB.stringUtf8 . mkUtfWord lg

mkUtfWord :: LetterGraph -> [Int] -> String
mkUtfWord lg inds = map (\i -> fst $ fromMaybe (error $ "null in mkUtfWord: " ++ show inds) $ M.lookup i lg) . reverse $ inds

mkBS :: String -> B.ByteString
mkBS = LB.toStrict . BB.toLazyByteString . BB.stringUtf8

oneMoreLetter :: LetterGraph -> WordList -> SuccF OneWordNode
oneMoreLetter lg words (OneWordNode is _) = goalNode ++ (filter validPrefix $ map mkNode nextLetters)
   where
      nextLetters = case is of []       -> M.keys lg
                               xs@(x:_) -> filter (not . flip elem xs) $ snd (lg M.! x)

      mkNode :: Int -> OneWordNode
      mkNode i = OneWordNode (i:is) Continue

      validPrefix :: OneWordNode -> Bool
      validPrefix n = not . T.null $ T.submap (mkWord lg $ view solution n) words

      -- add this node again as a child with goal=true if it is a valid word
      goalNode = filter (validWord lg words . view solution) [OneWordNode is Stop]

isGoal :: GoalF OneWordNode
isGoal (OneWordNode _ Stop) = True
isGoal _ = False

validWord :: LetterGraph -> WordList -> Solution -> Bool
validWord lg words inds = (T.member (mkWord lg inds) words) && length inds > 2

searchWords :: LetterGraph -> WordList -> [OneWordNode]
searchWords lg words = bfs (oneMoreLetter lg words) isGoal (OneWordNode [] Continue)

-- |Removes the letters of the solution from a letter graph. The
--  remaining letters "fall down" to take the place of the removed ones.
sinkField :: FieldWidth -> Solution -> LetterGraph -> LetterGraph
sinkField width inds lg = mkLetterGraph width . map snd . M.toList $ filledLg
   where
      height = (+1) . (`div` width) . fst . M.findMax $ lg

      indsS = IS.fromList inds

      -- we create a new, empty board of fields (filled with spaces).
      -- then we go through the old letter graph and insert its letters into the
      -- correct (=sunken) positions of the new graph.
      emptyLg = M.fromList [(i,' ') | i <- [0.. (width*height - 1)]]

      filledLg :: M.Map Int Char
      filledLg = M.foldlWithKey' (\m k (c,_) -> if IS.member k indsS then m else M.insert (sink k) c m) emptyLg lg 

      -- move a key down by however many fields below it are empty
      sink k = k + width * numEmptyBelow k

      -- the number of fields directly below a field which are empty.
      -- "empty" means "contained in the given solution", since those are supposed to
      -- be deleted.
      numEmptyBelow k = foldl' (\num j -> if (k `rem` width) == (j `rem` width) && j > k then num+1 else num) 0 inds

oneMoreWord :: FieldWidth -> WordList -> SuccF MultiWordNode
oneMoreWord width words (MultiWordNode lg sols remainingWords) = map mkSucc solutions
   where
      solutions = filter (flip IMS.member remainingWords . length) $ map (view solution) $ searchWords lg words

      mkSucc inds = MultiWordNode (sinkField width inds lg) (sols ++ [(inds, lg)]) (IMS.delete (length inds) remainingWords)

      -- trace' x = traceShow x x

isMultiWordGoal :: GoalF MultiWordNode
isMultiWordGoal = IMS.null . view remainingWords

searchManyWords :: FieldWidth -> LetterGraph -> WordList -> [WordLength] -> [MultiWordNode]
searchManyWords width lg words wl = bfs (oneMoreWord width words) isMultiWordGoal (MultiWordNode lg [] $ IMS.fromList wl)

main :: IO ()
main = do
   putStrLn "Enter file name:"
   words <- readWordList =<< getLine
   putStrLn "Enter field width: "
   width <- read <$> getLine
   whileJust_
      (do putStrLn "Enter letter (no separators, no newlines, spaces for empty fields) or :exit to quit: "
          line <- getLine
          return $ if line == ":exit" then Nothing else Just line)
      (\line -> do let lg = mkLetterGraph width line
                   putStrLn "Enter lengths of the words (comma-separated): "
                   lengths <- map read . splitOn "," <$> getLine
                   putStrLn "Searching solutions..."
                   let sols = searchManyWords width lg words lengths
                       showSolution n = putStrLn . intercalate " - " . map (uncurry $ flip mkUtfWord) . view solutions $ n
                       showDebug = putStrLn . show . map show . view solutions
                   mapM_ (\x -> showSolution x >> showDebug x) sols
                   --mapM_ putStrLn $ nub $ map (mkUtfWord lg . view solution) $ searchWords lg words
                   putStrLn "--------------------"
                   putStrLn "done!")
