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
import Control.Monad.Loops
import Control.Lens
import Data.List
import Data.Maybe
import Search

type LetterGraph = M.Map Int (Char, [Int])
type WordList = T.Trie ()
data Stop = Stop | Continue deriving (Eq, Show, Ord, Enum, Bounded, Read)

data Node = Node{
   _nodeSolution::[Int],
   _nodeStop::Stop}
   deriving (Eq, Show)

makeFields ''Node

mkLetterGraph :: Int -> [Char] -> LetterGraph
mkLetterGraph width cs = M.mapWithKey mkConns initMap
   where
      initMap :: LetterGraph
      initMap = M.fromList $ snd $ foldl' (\acc c -> if c == ' ' then acc else addLetter acc c) (0,[]) cs

      addLetter (i,xs) c = (i+1,(i,(c,[])):xs)

      mkConns :: Int -> (Char, [Int]) -> (Char, [Int])
      mkConns k = second $ const $ filter (flip M.member initMap) $ line width (-1) k ++ line width 0 k ++ line width 1 k

line :: Int -> Int -> Int -> [Int]
line width offset k = map (+ (width * offset)) $
   (if k `rem` width == 0   then [] else [k-1]) ++
   (if offset == 0          then [] else [k]) ++
   (if (k+1) `rem` width == 0 then [] else [k+1])

readWordList :: FilePath -> IO WordList
readWordList fp = T.fromList . map ((,()) . mkBS) . lines <$> readFile fp

mkWord :: LetterGraph -> [Int] -> B.ByteString
mkWord lg = LB.toStrict . BB.toLazyByteString . BB.stringUtf8 . mkUtfWord lg

mkUtfWord :: LetterGraph -> [Int] -> String
mkUtfWord lg inds = map (\i -> fst $ fromMaybe (error $ "null in mkUtfWord: " ++ show inds) $ M.lookup i lg) . reverse $ inds

mkBS :: String -> B.ByteString
mkBS = LB.toStrict . BB.toLazyByteString . BB.stringUtf8

oneMoreLetter :: LetterGraph -> WordList -> SuccF Node
oneMoreLetter lg words (Node is _) = goalNode ++ (filter validPrefix $ map mkNode nextLetters)
   where
      nextLetters = case is of []       -> M.keys lg
                               xs@(x:_) -> filter (not . flip elem xs) $ snd (lg M.! x)

      mkNode :: Int -> Node
      mkNode i = Node (i:is) Continue

      validPrefix :: Node -> Bool
      validPrefix n = not . T.null $ T.submap (mkWord lg $ view solution n) words

      -- add this node again as a child with goal=true if it is a valid word
      goalNode = filter (validWord lg words . view solution) [Node is Stop]

validWord :: LetterGraph -> WordList -> [Int]-> Bool
validWord lg words inds = (T.member (mkWord lg inds) words) && length inds > 2

isGoal :: Node -> Bool
isGoal (Node _ Stop) = True
isGoal _ = False

searchWords :: LetterGraph -> WordList -> [Node]
searchWords lg words = bfs (oneMoreLetter lg words) isGoal (Node [] Continue)

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
                   putStrLn "Searching solutions..."
                   mapM_ putStrLn $ nub $ map (mkUtfWord lg . view solution) $ searchWords lg words
                   putStrLn "--------------------"
                   putStrLn "done!")
