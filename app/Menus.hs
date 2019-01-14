module Menus where

import qualified TetrisData as TD

data Screen = Main
            | KeyBindings
            | Difficulty
            | PreGame
            | MidGame

data Entry = Entry
    { name :: String
    , action :: Action
    }

data Action = ChangeMenu Screen
            | GameAct TD.Delta

entries :: Screen -> [Entry]
entries Main = [ Entry "Start"    $ ChangeMenu PreGame
               , Entry "Keys"     $ ChangeMenu KeyBindings
               , Entry "Hardness" $ ChangeMenu Difficulty
               , Entry "Exit"     $ GameAct    TD.Exit
               ]
entries KeyBindings = []
entries Difficulty = []
entries PreGame = [ Entry "Go!"      $ GameAct TD.Start
                  , Entry "Hardness" $ ChangeMenu Difficulty
                  ]
entries MidGame = [ Entry "Resume" $ GameAct TD.Resume
                  , Entry "Cancel" $ GameAct TD.Cancel
                  ]
