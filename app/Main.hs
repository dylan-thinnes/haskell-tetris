module Main where

import Tetris (tetris)

import Lib
import UI.NCurses
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Array

hw :: Update ()
hw = drawString "Hello World!"

redOnBlack :: Window -> Curses ()
redOnBlack w = do
    id <- newColorID ColorRed ColorBlack 19
    updateWindow w $ setColor id

bold :: Window -> Curses () 
bold w = updateWindow w $ setAttribute AttributeBold True

boxify :: Window -> Curses ()
boxify w = updateWindow w b
    where
    b = drawBorder Nothing Nothing Nothing Nothing 
                   Nothing Nothing Nothing Nothing

type StartTime = Int
data DiscreteVector = DV StartTime (Int, Int)

type Tile = Bool
type User = (Integer, Integer)

data Game = Game
    { users :: M.Map Int User
    , board :: Array (Integer, Integer) Bool
    }

data Delta = Move Int (Integer, Integer)
           | Inspect
           | Interact

moveUser :: (Integer, Integer) -> Delta
moveUser = Move 0

applyDelta :: Game -> Delta -> Game
applyDelta g (Move uid (dx, dy)) = g { users = users' }
    where
        users' = M.adjust (\(x,y) -> (x+dx,y+dy)) uid (users g)
applyDelta g _ = g

applyDeltas :: Game -> [Delta] -> Game
applyDeltas = foldl applyDelta

tick :: Curses ()
tick = do
    return undefined
    --w <- defaultWindow
    --c <- getEvent w Nothing
    --case c of
    --  Just (EventSpecialKey k) ->
    --      updateWindow w $ case k of
    --                         KeyUpArrow ->    moveUser (0,-1)
    --                         KeyDownArrow ->  moveUser (0,1)
    --                         KeyLeftArrow ->  moveUser (-1,0)
    --                         KeyRightArrow -> moveUser (1,0)
    --                         _ -> return ()
    --  Just (EventCharacter _) -> return ()
    --  _ -> return ()

main :: IO ()
main = do
    tetris
    --runCurses colorTest
    --w <- runCurses defaultWindow
    --runCurses $ mapM_ (a w) [1..]

colorTest :: Curses ()
colorTest = do
    let colors = [(r,g,b) | r <- [0,680], g <- [0,680], b <- [0,680]] 
    forM_ colors $ \(r,g,b) -> printColor r g b
    forever render

printColor :: Integer -> Integer -> Integer -> Curses ()
printColor r g b = do
    defineColor (Color 255) r g b
    id <- newColorID (Color 255) (Color 255) 1
    w <- defaultWindow
    updateWindow w $ do
        setColor id
        drawText "X"

a :: Window -> Int -> Curses ()
a w _ = do
        redOnBlack w
        bold w
        boxify w
        setCursorMode CursorInvisible
        updateWindow w $ do
            moveCursor 1 1
            drawString "Hello"
        render
    --runCurses $ do
    --    redOnBlack w
    --    bold w
    --    boxify w
    --    setCursorMode CursorInvisible
    --    updateWindow w $ do
    --        drawString "Hello"
    --    render
    --    forever render
    --runCurses $ do
    --    w <- defaultWindow
    --    redOnBlack w
    --    bold w
    --    boxify w
    --    setCursorMode CursorInvisible
    --    updateWindow w $ moveCursor 1 1
    --    render
    --    forever (tick >> render)

