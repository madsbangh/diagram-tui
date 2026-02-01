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

data Cell = Box Box | Lines

data Box = MkBox
  { label :: String,
    up ::
      Connection,
    down :: Connection,
    left :: Connection,
    right :: Connection
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
  let mkBox s = Box $ MkBox s ArrowIn Line ArrowIn Line
   in renderColumn
        [mkBox "Hello", mkBox "Other, longer box."]
        <+> renderColumn
          [mkBox "world", mkBox "Fourth"]

box :: Bool -> String -> Widget ()
box True = withBorderStyle unicodeBold . border . padAll 1 . str
box False = withBorderStyle unicode . border . padAll 1 . str

toWidget :: Int -> Bool -> Cell -> Widget ()
toWidget _ _ Lines = fill '+'
toWidget colWidth selected (Box b) =
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

-- ◄ ► ▲ ▼ ─ │ │ ─ ┬ ┴ ┼ ╭ ╮ ╯ ╰
