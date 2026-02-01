{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad
import Graphics.Vty

-- Using a fixed height per cell, so things line up across columns
-- 1 text line + 2 padding + 2 border + 2 connections = 7
cellHeight :: Int
cellHeight = 7

type Model = ()

data Cell = Box Box | Junction Junction

data Box = MkBox
  { label :: String,
    up ::
      Connection,
    down :: Connection,
    left :: Connection,
    right :: Connection
  }

data Junction = MkJunction
  { jUp :: Bool,
    jDown :: Bool,
    jLeft :: Bool,
    jRight :: Bool
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
  let boxTopLeft = Box $ MkBox "First" None Line None Line
      boxTopRight = Box $ MkBox "Second" None None ArrowIn None
      boxBottomLeft = Box $ MkBox "Third" ArrowIn None None None
      boxBottomRight = Box $ MkBox "Fourth (end)" ArrowIn None None None
      junctionLeft = Junction $ MkJunction True True False True
      junctionRight = Junction $ MkJunction False True True False
   in renderColumn [boxTopLeft, junctionLeft, boxBottomLeft]
        <+> renderColumn [boxTopRight, junctionRight, boxBottomRight]

box :: Bool -> String -> Widget ()
box True = withBorderStyle unicodeBold . border . padAll 1 . str
box False = withBorderStyle unicode . border . padAll 1 . str

toWidget :: Int -> Bool -> Cell -> Widget ()
toWidget colWidth selected (Box b) = toBoxWidget colWidth selected b
toWidget _ selected (Junction j) = toJunctionWidget selected j

toBoxWidget :: Int -> Bool -> Box -> Widget ()
toBoxWidget colWidth selected b =
  let content = label b
      boxWidget = box selected content
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

toJunctionWidget :: Bool -> Junction -> Widget ()
toJunctionWidget selected j =
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
        vBox (map (vLimit cellHeight . hCenter . toWidget cw False) column)
