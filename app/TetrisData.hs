{-# LANGUAGE TemplateHaskell #-}
module TetrisData where

import Control.Lens
import Data.Time.Clock (UTCTime)
import TileGrid
import UI.NCurses (Window)

data Game = Game
    { _piece :: Piece
    , _board :: TileGrid
    , _gravityRemaining :: Int
    , _prevTick :: UTCTime
    , _currTick :: UTCTime
    }

data Settings = Settings
    { _gravity :: Int
    , _width :: Integer
    , _height :: Integer
    , _window :: Window
    }

data Delta = MovePiece Direction
           | WarpPiece
           | LockPiece
           | HoldPiece
           | RotatePiece
           | Tick UTCTime
           -- Game Management
           | Start
           | Pause
           | Resume
           | Cancel
           | Exit
           -- Menu Management
           | NextSelection
           | PrevSelection
           | LastSelection
           | EnterSelection
           -- Debugging
           | ChangePiece

data Input = Select
           | ArrowLeft | ArrowRight | ArrowUp | ArrowDown
           | HardDown
           | Rotate90 | Rotate180 | Rotate270
           | Escape

data Mode = Menu MenuType
          | Running

data MenuType = Main
              | KeyBindings
              | Difficulty
              | PreGame
              | MidGame

makeLenses ''Game
makeLenses ''Settings
