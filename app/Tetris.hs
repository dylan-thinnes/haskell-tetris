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
    piece = Piece (0,0) L Up
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
    clearPiece
    consumeDelta d
    drawPiece
    lift render

consumeDelta :: Delta -> Tick ()
consumeDelta RotatePiece = piece.orientation %= cycleC
consumeDelta ChangePiece = piece.shape %= cycleC
consumeDelta (MovePiece GoRight) = tryMovePieceRel 1 0 >> return ()
consumeDelta (MovePiece GoLeft)  = tryMovePieceRel (-1) 0 >> return ()
consumeDelta (MovePiece GoDown)  = do
    tryMovePieceRel 0 1
    resetGravity
consumeDelta (Tick time) = do
    rem <- decrementGravity
    if rem == 0
    then do
        tryMovePieceRel 0 1
        resetGravity
    else return ()
consumeDelta WarpPiece = do
    h <- view height
    (_,y) <- gets $ view $ piece.position
    forM_ [y..h] $ \_ -> tryMovePieceRel 0 1
consumeDelta _ = return ()

tryMovePieceRel :: Integer -> Integer -> Tick Bool
tryMovePieceRel dx dy = do
    (x,y) <- gets $ view $ piece.position
    let x' = dx + x
    let y' = dy + y
    tryMovePieceAbs x' y'

validCoordinate :: Integer -> Integer -> Tick Bool
validCoordinate x y = do
    mx <- view width
    my <- view height
    return $ between x 0 mx && between y 0 my

tryMovePieceAbs :: Integer -> Integer -> Tick Bool
tryMovePieceAbs x y = do
    p <- gets $ view $ piece
    let maxX = pieceWidth p + x
    let maxY = pieceHeight p + y
    minValid <- validCoordinate x y
    maxValid <- validCoordinate maxX maxY
    if minValid && maxValid
    then do
        piece.position .= (x,y)
        return True
    else return False

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
decrementGravity = gravityRemaining -= 1 >> gets (view gravityRemaining)

resetGravity :: Tick ()
resetGravity = view gravity >>= (gravityRemaining .=)
