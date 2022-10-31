module Main where




import Control.Concurrent 
import Game.Chess (fromFEN, Position, startpos, legalPlies, fromUCI, toUCI, Ply, doPly, unsafeDoPly)

import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)
import Data.List (foldl')

import Control.Monad.State

import ChessEngine

main :: IO ()
main = do 
        hSetBuffering stdin LineBuffering
        hSetBuffering stdout LineBuffering
        putStrLn "Test"
        ch <- newChan
        _ <- forkIO (worker ch)
        --xs <- getChanContents ch
        xs <- readChan ch
        mapM_ (putStrLn . unlines ) $ runGame xs


runGame :: [String] -> [[String]]
runGame xs = evalState (processCommands xs) (startpos :: Position)
                

 
processCommands :: [String] -> State Position [[String]]

processCommands [] = do return [[]]
processCommands (x:xs) = do let tokens = words x 
                            let command = head tokens
                            let args = tail tokens 
                            response <-  case command of "position"   ->  do pos <- get
                                                                             let lastarg = last args 
                                                                             case lastarg of "startpos" -> return [""]
                                                                                             _ -> updatePosition pos lastarg

                                                         "go"         ->  playBestMove

                                                         "uci"        -> return ["id name ChessEngine", "id author Purewater", "uciok"]
                                                         "debug"      -> return []
                                                         "isready"    -> return ["readyok"]
                                                         "ucinewgame" -> return []
                                                         _            -> return []


                            rs <- processCommands xs
                            return $ response:rs 
updatePosition :: Position -> String -> State Position [String]
updatePosition pos moveUCI = do let move = (fromUCI pos moveUCI)
                                put $ case move of Nothing -> pos
                                                   Just m -> doPly pos m
                                return [""]
                             
                             
                                        


playBestMove :: State Position [String]

playBestMove = do pos <- get
                  let move = bestMove pos
                  put $ unsafeDoPly pos move
                  return $ [generateCommand move]

                  



worker :: Chan [String] -> IO () 
worker ch = do
        command <- getContents
        writeChan ch (lines command)

getJustVal :: Maybe a -> a
getJustVal (Just x) = x
getJustVal Nothing = error $ "Nothing Error"


getBestMovesFromArgs :: String -> [String] -> String
getBestMovesFromArgs posUCI movesUCI = maybe "" (flip movesToCommand $  moves)  pos
                     where pos = getPosition posUCI
                           movesToCommand position = generateCommand . bestMove . applyMoves position
                           moves = mapWithState (getJustVal pos) fromUCI doPly movesUCI

mapWithState :: c -> (c-> a -> Maybe b) -> (c -> b ->  c) -> [a] -> [b] 

mapWithState  _ _ _  [] = []
mapWithState st f g (x:xs) = move : mapWithState (g st move) f g xs
                                where move = getJustVal $ f st x

getPosition :: String -> Maybe Position
getPosition "startpos" = return startpos
getPosition pos = fromFEN pos 

applyMoves :: Position -> [Ply] -> Position
applyMoves pos moves = foldl' doPly pos moves

generateCommand :: Ply -> String
generateCommand move = unwords ["bestmove", toUCI move]

--bestMove :: Position -> Ply
--bestMove pos = head $ legalPlies pos



