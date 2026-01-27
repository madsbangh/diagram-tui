{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Table
import Control.Monad
import Data.List (intersperse)
import Data.Monoid ((<>))
import GHC.Exts (the)
import Graphics.Vty

type Model = Bool

main :: IO ()
main = void $ defaultMain app False

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
drawApp isRight = [appWidget isRight]

updateApp :: BrickEvent () e -> EventM () Model ()
updateApp (VtyEvent (EvKey (KChar 'h') [])) = modify (const False)
updateApp (VtyEvent (EvKey (KChar 'l') [])) = modify (const True)
updateApp (VtyEvent (EvKey (KChar 'q') [])) = halt
updateApp (VtyEvent (EvKey KEsc [])) = halt
updateApp _ = return ()

appWidget :: Bool -> Widget ()
appWidget isRight =
  let box borderStyle = withBorderStyle borderStyle . border . padAll 1 . str
      leftStyle = if isRight then unicode else unicodeBold
      rightStyle = if isRight then unicodeBold else unicode
      hello = box leftStyle "Hello"
      arrow1 = str $ map ($ H L) [makeArrow, makeLine, makeLine]
      arrow2 = str $ intersperse '\n' $ map ($ V U) [makeArrow, makeLine]
      line = str $ intersperse '\n' $ map ($ V U) [makeLine, makeLine]
      world = box rightStyle "World"
   in center $
        renderTable $
          centeredBorderlessTable
            [ [hello, arrow1, world],
              [arrow2, emptyWidget, emptyWidget],
              [line, emptyWidget, emptyWidget],
              [hello, emptyWidget, emptyWidget]
            ]

centeredBorderlessTable :: [[Widget ()]] -> Table ()
centeredBorderlessTable =
  surroundingBorder False
    . rowBorders False
    . columnBorders False
    . setDefaultColAlignment AlignCenter
    . setDefaultRowAlignment AlignMiddle
    . table

makeArrow :: Neighbor -> Char
makeArrow (H L) = '◄'
makeArrow (H R) = '►'
makeArrow (V U) = '▲'
makeArrow (V D) = '▼'

makeLine :: Neighbor -> Char
makeLine (V _) = '│'
makeLine (H _) = '─'

makeCorner :: VNeighbor -> HNeighbor -> Char
makeCorner U L = '┘'
makeCorner D L = '┐'
makeCorner U R = '└'
makeCorner D R = '┌'

data Neighbor = V VNeighbor | H HNeighbor

data VNeighbor = U | D

data HNeighbor = L | R
