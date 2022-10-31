
module ChessEngine where

--import Control.Concurrent
--import Control.Monad
import Control.Parallel.Strategies
import Control.Monad.ST
import Game.Chess (legalPlies, toFEN, unsafeDoPly, doPly, Color(White, Black), Ply, Position, color)
import Game.Chess.Tree 
--import Data.Maybe
import Data.Char

import Data.Tree 

import Data.List
import Data.Ord
import Data.Hashable

import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
 
type HashTable s k v = C.HashTable s k v



bestMove :: Position -> Ply
bestMove pos =  fst $ maximumBy (comparing snd) $ zip moves $ map (evaluate pos) moves
                where moves = legalPlies pos


---positionTree :: (Ply, Position) -> Tree (Ply, Position)
--positionTree (move, pos) = Node (move, pos) $ positionForest (move, pos)


--positionForest :: (Ply, Position) -> Forest (Ply, Position)
--positionForest (move, pos) =  positionTree <$> zip (legalPlies pos) (unsafeDoPly pos <$> legalPlies pos)



--evaluatePositions :: Tree Position -> Tree Int
--evaluatePositions (Node pos tree) = Node (pos, countMaterial pos) $ map evaluatePositions tree



countMaterial :: Position -> Color -> Integer
countMaterial pos col = sum $ map pieceValues lowerCaseSide 
        where filter_func = case col of White -> isUpper
                                        Black -> isLower
              pieceValues x = case x of 'p' -> 1
                                        'n' -> 3
                                        'b' -> 3
                                        'r' -> 5
                                        'q' -> 9
                                        _ -> 0
              fenStr = head . words .toFEN $ pos
              lowerCaseSide = map toLower (filter filter_func fenStr)
             



prune :: Int -> Tree a -> Tree a 
prune 0 (Node x _) = Node x []
prune n (Node x sub) = Node x (map (prune (n-1)) sub)


insertPos :: (Hashable k) => HashTable s k v -> k -> v -> ST s v
insertPos tab key val = do H.insert tab key val
                           return val




pmap _ [] = []
pmap f (x:xs) = runEval $ do x' <- rpar (f x)
                             xs' <- rpar (pmap f xs)
                             return $ (x':xs')


maximize :: Position -> Tree Ply -> ST s Integer
maximize pos (Node move []) = do return $ countMaterial (doPly pos move) col
                                where col = color pos
maximize pos (Node move sub) = do evals <- sequence $ pmap (minimize $ unsafeDoPly pos move) sub
                                  let searchEval = maximum evals
                                  visited <- visitedPositions 
                                  storedEval <- H.lookup visited pos
                                  case storedEval of Nothing     -> do insertPos visited pos searchEval
                                                     Just eval   -> return eval


                                


minimize :: Position -> Tree Ply -> ST s Integer 
minimize pos (Node move []) = do return $ -1 * ( countMaterial (doPly pos move) col ) 
                                where col = color pos
minimize pos (Node move sub) = do evals <- sequence $ pmap (maximize $ unsafeDoPly pos move) sub
                                  let searchEval = maximum evals
                                  visited <- visitedPositions 
                                  storedEval <- H.lookup visited pos
                                  case storedEval of Nothing     -> do insertPos visited pos searchEval
                                                     Just eval   -> return eval 






visitedPositions :: ST s (HashTable s Position Integer)
visitedPositions = H.newSized 1000
                     

evaluate :: Position -> Ply -> Integer
evaluate pos move =  runST $ maximize pos (prune 4 (plyTree pos move))


