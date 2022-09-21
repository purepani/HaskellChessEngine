

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad



main = do 
        ch <- newChan
        forkIO (worker ch)
        xs <- getChanContents ch
        mapM_ (putStr . unlines . processCommand) xs


worker ch = forever $ do
                command<-getLine 
                writeChan ch command

processCommand :: String -> [String]
processCommand command = case command_name of "uci" -> ["id name engine_name Satwik", "uciok"]
                                              "debug" -> []
                                              "isready" -> ["readyok"]
                                              "" -> []
                         where command_name =  head (words command)
