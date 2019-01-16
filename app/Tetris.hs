module Tetris 
    where

import UI.NCurses
import Control.Monad
import Control.Monad.RWS.Strict
import qualified Data.Map.Strict as M
import Control.Concurrent (forkIO, threadDelay)
import CloseableChannel
import Data.Time.Clock (getCurrentTime)
import qualified Data.Sequence as S
import TetrisData
import TileGrid
import Rendering
import Utils
import Control.Lens hiding (Empty)

-- Initializing and running the game
tetris :: IO ()
tetris = runCurses $ do
    w <- initialize
    let settings = Settings { _gravity = 60
                            , _width = 10
                            , _height = 20
                            , _window = w
                            }
    let g = createGameFromSettings settings
    channel <- liftIO $ newChan
    liftIO $ forkIO $ logic channel
    deltas <- liftIO $ getChanContents channel
    evalRWST (mapM_ tick deltas) settings g
    return ()

initialize :: Curses Window
initialize = do
    w <- defaultWindow
    setCursorMode CursorInvisible
    setEcho False
    updateWindow w $ do
        resizeWindow 22 12
        boxify
        setAttribute AttributeBold True
    return w

createGameFromSettings :: Settings -> Game
createGameFromSettings settings = Game piece board gravityRemaining prevTick currTick
    where
    w = view width settings
    h = view height settings
    piece = Piece (0,0) L Rot0
    board = emptyGrid w h
    gravityRemaining = view gravity settings
    prevTick = undefined
    currTick = undefined

logic :: CloseableChannel Delta -> IO ()
logic chan = do
    forkIO $ forever $ do
        t <- getCurrentTime
        writeChan chan (Tick t)
        threadDelay 16666
    forkIO $ runCurses $ forever $ do
        w <- defaultWindow
        c <- getEvent w Nothing
        case c of
          Just (EventSpecialKey c) -> case c of
            KeyUpArrow    -> liftIO $ writeChan chan $ RotatePiece
            KeyLeftArrow  -> liftIO $ writeChan chan $ MovePiece GoLeft
            KeyRightArrow -> liftIO $ writeChan chan $ MovePiece GoRight
            KeyDownArrow  -> liftIO $ writeChan chan $ MovePiece GoDown
            _   -> return ()
          Just (EventCharacter c) -> case c of
            ' ' -> liftIO $ writeChan chan WarpPiece
            'q' -> liftIO $ writeChan chan ChangePiece
            _   -> return ()
          _ -> return ()
    return ()
    
type Tick a = RWST Settings (S.Seq String) Game Curses a

tick :: Delta -> Tick ()
tick d = do
    consumeDelta d
    lift render

consumeDelta :: Delta -> Tick ()
consumeDelta RotatePiece         = tryModPiece (Mod 0 0 Rot90        ) >> return ()
consumeDelta ChangePiece         = redrawPiece (piece.shape %= cycleC) >> return ()
consumeDelta (MovePiece GoRight) = tryModPiece (Mod 1 0 Rot0         ) >> return ()
consumeDelta (MovePiece GoLeft)  = tryModPiece (Mod (-1) 0 Rot0      ) >> return ()
consumeDelta (MovePiece GoDown)  = do
    tryModPiece $ Mod 0 1 Rot0
    resetGravity
consumeDelta (Tick time) = do
    rem <- decrementGravity
    if rem == 0
    then do
        tryModPiece (Mod 0 1 Rot0)
        resetGravity
    else return ()
consumeDelta WarpPiece = do
    h <- view height
    y <- gets $ view $ piece.position._1
    redrawPiece $ forM_ [y..h] $ const $ tryModPiece (Mod 0 1 Rot0)
consumeDelta _ = return ()

redrawPiece :: Tick a -> Tick a
redrawPiece t = clearPiece >> t <* drawPiece

tryModPiece :: Mod -> Tick Bool
tryModPiece m = do
    p <- gets $ view piece
    let p' = applyMod m p
    mx <- view width
    my <- view height
    b <- gets $ view board
    if pieceWithin p' mx my && not (collision (pieceToTilegrid p) b)
    then do
        redrawPiece $ piece .= p'
        return True
    else do
        return False

clearPiece :: Tick ()
clearPiece = do
    p <- gets $ view $ piece
    w <- view window
    let grid = pieceToTilegrid p
    lift $ runRender (clearGrid grid) w

drawPiece :: Tick ()
drawPiece = do
    p <- gets $ view $ piece
    w <- view window
    let grid = pieceToTilegrid p
    lift $ runRender (drawGrid grid) w

decrementGravity :: Tick Int
decrementGravity = gravityRemaining <-= 1

resetGravity :: Tick ()
resetGravity = view gravity >>= (gravityRemaining .=)
