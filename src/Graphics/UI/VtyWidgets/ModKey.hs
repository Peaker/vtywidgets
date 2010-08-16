{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.VtyWidgets.ModKey
    (ModKey(..), showKey)
where

import qualified Graphics.Vty  as Vty

data ModKey = ModKey ![Vty.Modifier] !Vty.Key
  deriving (Eq, Ord)

showKey :: Vty.Key -> String
showKey key = case key of
  Vty.KEsc        -> "Escape"
  Vty.KFun n      -> 'F' : show n
  Vty.KBackTab    -> "BackTab"
  Vty.KPrtScr     -> "PrintScreen"
  Vty.KPause      -> "Pause"
  Vty.KASCII ' '  -> "Space"
  Vty.KASCII '\t' -> "Tab"
  Vty.KASCII x    -> [x]
  Vty.KBS         -> "Backspace"
  Vty.KIns        -> "Insert"
  Vty.KHome       -> "Home"
  Vty.KPageUp     -> "PageUp"
  Vty.KDel        -> "Delete"
  Vty.KEnd        -> "End"
  Vty.KPageDown   -> "PageDown"
  Vty.KNP5        -> "NP5"
  Vty.KUp         -> "Up"
  Vty.KMenu       -> "Menu"
  Vty.KLeft       -> "Left"
  Vty.KDown       -> "Down"
  Vty.KRight      -> "Right"
  Vty.KEnter      -> "Enter"

instance Show ModKey where
  show (ModKey mods key) = unwords (modsStr ++ [showKey key])
    where
      modsStr = map (drop 1 . show) mods
