

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import System.Posix (PathVar(PathNameLimit))
import Game.Chess



main = do 
        ch <- newChan
        forkIO (worker ch)
        xs <- getChanContents ch
        mapM_ (putStr . unlines . processCommand) xs


worker ch = forever $ do
                command<-getLine 
                writeChan ch command

processCommand :: String -> [String]
processCommand command = case command_name of "uci" -> ["id name engine_name Chess Engine", "id author Satwik Path","uciok"]
                                              "debug" -> []
                                              "isready" -> ["readyok"]
                                              "ucinewgame" -> []
                                              "" -> []
                                              x -> []
                         where command_name =  head (words command)

parsePositionCommand :: 
