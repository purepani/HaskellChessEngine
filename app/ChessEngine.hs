
module ChessEngine where

--import Control.Concurrent
--import Control.Monad
import Control.Parallel.Strategies
import Control.Monad.ST as S
import qualified Control.Monad.ST.Lazy as L
import Game.Chess (legalPlies, toFEN, unsafeDoPly, doPly, Color(White, Black), Ply, Position, color, opponent)
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



countMaterial :: Position -> Color -> Int
countMaterial pos col = sum $ map pieceValues lowerCaseSide 
        where filter_func = case col of White -> isUpper
                                        Black -> isLower
              pieceValues x = case x of 'p' -> 100
                                        'n' -> 300
                                        'b' -> 300
                                        'r' -> 500
                                        'q' -> 900
                                        _ -> 0
              fenStr = head . words .toFEN $ pos
              lowerCaseSide = map toLower (filter filter_func fenStr)
             

evalPos :: Position -> Int
evalPos pos = -(countMaterial pos White) + (countMaterial pos Black)


prune :: Int -> Tree a -> Tree a 
prune 0 (Node x _) = Node x []
prune n (Node x sub) = Node x (map (prune (n-1)) sub)


insertPos :: (Hashable k) => HashTable s k v -> k -> v -> ST s v
insertPos tab key val = do H.insert tab key val
                           return val






--minimax :: Position -> Tree Ply -> L.ST s Integer
--minimax pos (Node move []) = do return $ countMaterial (doPly pos move) col
                                --where col = color pos
--minimax pos (Node move sub) = do evals <- sequence $ map (minimax $ unsafeDoPly pos move) sub
                                 --let searchEval = negate . maximum $ using evals $ parList rpar
                                 --visited <- visitedPositions 
                                 --storedEval <- L.strictToLazyST $ H.lookup visited pos
                                 --case storedEval of Nothing     -> do L.strictToLazyST $ insertPos visited pos searchEval
                                                    --Just eval   -> return eval
                                                    --


                                
minimax :: Position -> Tree Ply -> Int
minimax pos (Node move []) =  evalPos $ doPly pos move 
minimax pos (Node move sub) = let evals = map (minimax $ unsafeDoPly pos move) sub
                                  eval = negate . maximum $ using evals $ parList rpar
                              in eval


--minimax_ab :: Int -> Int -> Position -> Tree Ply -> Int
--minimax_ab a b pos (Node move []) = min b $ max a $ evalPos (doPly pos move) 
--minimax_ab a b pos (Node move sub) =  
--        where val = minBound 



--  
minimax_ab2 :: Int -> Int -> Position -> Tree Ply -> Int
minimax_ab2 alpha beta pos (Node move []) = alpha `max` x `min` beta
                                where x = evalPos $ unsafeDoPly pos move
minimax_ab2 alpha beta pos (Node move moveTreeLs) = cmx alpha moveTreeLs
        where cmx a lstree = head . filter (beta<=) $ scanl updateAlpha a lstree 
                where updateAlpha alph moveTree = -(minimax_ab2 (-beta) (-alph) (unsafeDoPly pos move) moveTree)
          --cmx a []  = a
          --cmx a (t:ts) | a'>=beta     = a'
                       -- | otherwise = cmx a' ts
                       --where a' = -(minimax_ab (-beta) (-a) (unsafeDoPly pos move) t)

minimax_ab :: Int -> Int -> Position -> Tree Ply -> Int
minimax_ab alpha beta pos (Node move []) = alpha `max` x `min` beta
                                where x = evalPos $ unsafeDoPly pos move
minimax_ab alpha beta pos (Node move moveTreeLs) = cmx alpha moveTreeLs
        where  newPos = unsafeDoPly pos move 
               cmx a []  = a
               cmx a (t:ts) | a'>=beta     = a'
                            | otherwise = cmx a' ts
                                 where a' = -(minimax_ab (-beta) (-a) newPos t)



minimax_ab_run :: Position -> Tree Ply -> Int
minimax_ab_run = minimax_ab minBound maxBound


visitedPositions :: L.ST s (HashTable s Position Int)
visitedPositions = L.strictToLazyST $ H.newSized 1000
                     

evaluate :: Position -> Ply -> Int
evaluate pos move =  minimax_ab_run pos (prune 4 (plyTree pos move))


