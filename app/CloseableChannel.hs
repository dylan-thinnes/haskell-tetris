module CloseableChannel 
    ( CloseableChannel
    , closeChan
    , newChan
    , writeChan
    , readChan
    , dupChan
    , getChanContents
    , writeList2Chan
    ) where

import Data.Maybe (fromJust, isJust)
import qualified Control.Concurrent.Chan as C

type CloseableChannel a = C.Chan (Maybe a)

closeChan :: CloseableChannel a -> IO ()
closeChan channel = C.writeChan channel Nothing

newChan :: IO (CloseableChannel a)
newChan = C.newChan

writeChan :: CloseableChannel a -> a -> IO ()
writeChan channel = C.writeChan channel . Just

readChan :: CloseableChannel a -> IO (Maybe a)
readChan channel = C.readChan channel >>= f
    where
    f x = case x of
        Nothing -> C.writeChan channel Nothing >> return Nothing
        a       -> return a
        

dupChan :: CloseableChannel a -> IO (CloseableChannel a)
dupChan = C.dupChan

getChanContents :: CloseableChannel a -> IO [a]
getChanContents channel = C.getChanContents channel >>= return . map fromJust . takeWhile isJust

writeList2Chan :: CloseableChannel a -> [a] -> IO ()
writeList2Chan ch = mapM_ (writeChan ch)
