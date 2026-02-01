{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad
import Graphics.Vty

type Model = ()

data Cell = Box Box | Junction Junction

data Box = MkBox
  { label :: String,
    up :: Connection,
    down :: Connection,
    left :: Connection,
    right :: Connection,
    bSelected :: Bool
  }

data Junction = MkJunction
  { jUp :: Bool,
    jDown :: Bool,
    jLeft :: Bool,
    jRight :: Bool,
    jSelected :: Bool
  }

data Connection = None | Line | ArrowIn

main :: IO ()
main = void $ defaultMain app ()

app :: App Model e ()
app =
  App
    { appDraw = drawApp,
      appStartEvent = return (),
      appHandleEvent = updateApp,
      appAttrMap = const (attrMap defAttr []),
      appChooseCursor = neverShowCursor
    }

drawApp :: Model -> [Widget ()]
drawApp _ = [appWidget ()]

updateApp :: BrickEvent () e -> EventM () Model ()
updateApp (VtyEvent (EvKey key [])) = case key of
  (KChar 'q') -> halt
  KEsc -> halt
  _ -> return ()
updateApp _ = return ()

appWidget :: Model -> Widget ()
appWidget _ =
  let boxTopLeft = Box $ MkBox "First" None Line None Line True
      boxTopRight = Box $ MkBox "Second" None None ArrowIn None False
      boxBottomLeft = Box $ MkBox "Third" ArrowIn None None None False
      boxBottomRight = Box $ MkBox "Fourth (end)" ArrowIn None None None False
      junctionLeft = Junction $ MkJunction True True False True False
      junctionRight = Junction $ MkJunction False True True False False
   in renderColumn [boxTopLeft, junctionLeft, boxBottomLeft]
        <+> renderColumn [boxTopRight, junctionRight, boxBottomRight]

box :: Bool -> String -> Widget ()
box True = withBorderStyle unicodeBold . border . padAll 1 . str
box False = withBorderStyle unicode . border . padAll 1 . str

toWidget :: Int -> Cell -> Widget ()
toWidget colWidth (Box b) = toBoxWidget colWidth b
toWidget _ (Junction j) = toJunctionWidget j

toBoxWidget :: Int -> Box -> Widget ()
toBoxWidget colWidth b =
  let content = label b
      boxWidget = box (bSelected b) content
      extraWidth = colWidth - boxWidth content
      upConn = case up b of
        None -> str $ replicate colWidth ' '
        Line -> hCenter (str "│")
        ArrowIn -> hCenter (str "▼")
      downConn = case down b of
        None -> str $ replicate colWidth ' '
        Line -> hCenter (str "│")
        ArrowIn -> hCenter (str "▲")
      leftConn = str $ case left b of
        None -> spaces
        Line -> hLine ++ "─"
        ArrowIn -> hLine ++ "►"
      rightConn = str $ case right b of
        None -> spaces
        Line -> "─" ++ hLine
        ArrowIn -> "◄" ++ hLine
      hLine = replicate (extraWidth `div` 2) '─'
      spaces = replicate (extraWidth `div` 2 + 1) ' '
   in upConn
        <=> ( vCenter leftConn
                <+> boxWidget
                <+> vCenter rightConn
            )
        <=> downConn

toJunctionWidget :: Junction -> Widget ()
toJunctionWidget j =
  let centerSymbol = str $ case (jUp j, jDown j, jLeft j, jRight j) of
        (False, False, False, False) -> " "
        (False, False, False, True) -> "?"
        (False, False, True, False) -> "?"
        (False, False, True, True) -> "─"
        (False, True, False, False) -> "?"
        (False, True, False, True) -> "╭"
        (False, True, True, False) -> "╮"
        (False, True, True, True) -> "┬"
        (True, False, False, False) -> "?"
        (True, False, False, True) -> "╰"
        (True, False, True, False) -> "╯"
        (True, False, True, True) -> "┴"
        (True, True, False, False) -> "│"
        (True, True, False, True) -> "├"
        (True, True, True, False) -> "┤"
        (True, True, True, True) -> "┼"
      vLine c = hCenter $ hLimit 1 $ vLimit 3 $ fill c
      topLine =
        if jUp j
          then vLine '│'
          else vLine ' '
      bottomLine =
        if jDown j
          then vLine '│'
          else vLine ' '
      leftLine =
        if jLeft j
          then fill '─'
          else fill ' '
      rightLine =
        if jRight j
          then fill '─'
          else fill ' '
   in topLine <=> (leftLine <+> centerSymbol <+> rightLine) <=> bottomLine

boxWidth :: String -> Int
boxWidth s = textWidth s + 6 -- contents + 2 * padding + 2 * border + 2 * border

columnWidth :: [Cell] -> Int
columnWidth column =
  let boxTexts [] = []
      boxTexts (c : cs) = case c of
        Box b -> label b : boxTexts cs
        _ -> boxTexts cs
   in maximum (0 : map boxWidth (boxTexts column))

renderColumn :: [Cell] -> Widget ()
renderColumn column =
  Widget Fixed Fixed $ do
    let cw = columnWidth column
    render $
      hLimit cw $
        vBox (map (vLimit 7 . hCenter . toWidget cw) column)
