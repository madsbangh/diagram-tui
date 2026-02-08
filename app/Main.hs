{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad
import Data.Map hiding (map)
import Data.Maybe
import Graphics.Vty
import Prelude hiding (head, lookup)

data Model = Model
  { grid :: Grid,
    selectedCell :: CellCoord
  }

type Grid = Map (Int, Int) Cell

type CellCoord = (Int, Int)

data Cell = Box Box | Junction Junction

data Box = MkBox
  { label :: String,
    up :: Connection,
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

data Connection = None | Line | ArrowIn deriving (Eq)

type RenderModel = [RenderColumn]

type RenderColumn = [RenderCell]

data RenderCell = RenderCell {cell :: Cell, selected :: Bool}

main :: IO ()
main =
  let startBox = Box $ MkBox "Start" None None None Line
      junction = Junction $ MkJunction False False True True
      endBox = Box $ MkBox "End" None Line ArrowIn None
      bottomBox = Box $ MkBox "Another box" ArrowIn None None None
      cell1 = ((0, 0), startBox)
      cell2 = ((1, 0), junction)
      cell3 = ((2, 0), endBox)
      cell4 = ((2, 1), bottomBox)
   in void $
        defaultMain
          app
          ( Model
              { grid = fromList [cell1, cell2, cell3, cell4],
                selectedCell = (2, 0)
              }
          )

app :: App Model e ()
app =
  App
    { appDraw = drawApp,
      appStartEvent = return (),
      appHandleEvent = updateApp,
      appAttrMap = const (attrMap defAttr [(selectedAttr, black `on` cyan)]),
      appChooseCursor = neverShowCursor
    }

selectedAttr :: AttrName
selectedAttr = attrName "selected"

drawApp :: Model -> [Widget ()]
drawApp m = [appWidget $ toRenderModel m]

updateApp :: BrickEvent () e -> EventM () Model ()
updateApp (VtyEvent (EvKey key [])) = case key of
  (KChar 'h') -> modify moveSelectionLeft
  (KChar 'l') -> modify moveSelectionRight
  (KChar 'k') -> modify moveSelectionUp
  (KChar 'j') -> modify moveSelectionDown
  (KChar 'd') -> modify deleteSelected
  (KChar 'x') -> modify deleteSelected
  (KChar 'q') -> halt
  KEsc -> halt
  _ -> return ()
updateApp _ = return ()

moveSelectionLeft :: Model -> Model
moveSelectionLeft m@Model {grid, selectedCell = (x, y)}
  | x > minX grid - selectionMargin = m {selectedCell = (x - 1, y)}
  | otherwise = m

moveSelectionRight :: Model -> Model
moveSelectionRight m@Model {grid, selectedCell = (x, y)}
  | x < maxX grid + selectionMargin = m {selectedCell = (x + 1, y)}
  | otherwise = m

moveSelectionUp :: Model -> Model
moveSelectionUp m@Model {grid, selectedCell = (x, y)}
  | y > minY grid - selectionMargin = m {selectedCell = (x, y - 1)}
  | otherwise = m

moveSelectionDown :: Model -> Model
moveSelectionDown m@Model {grid, selectedCell = (x, y)}
  | y < maxY grid + selectionMargin = m {selectedCell = (x, y + 1)}
  | otherwise = m

selectionMargin :: Int
selectionMargin = 2

minX :: Grid -> Int
minX = minCoord fst

minY :: Grid -> Int
minY = minCoord snd

maxX :: Grid -> Int
maxX = maxCoord fst

maxY :: Grid -> Int
maxY = maxCoord snd

minCoord :: ((Int, Int) -> Int) -> Grid -> Int
minCoord selector = minimum . map selector . keys

maxCoord :: ((Int, Int) -> Int) -> Grid -> Int
maxCoord selector = maximum . map selector . keys

deleteSelected :: Model -> Model
deleteSelected m@Model {grid, selectedCell} =
  case Data.Map.lookup selectedCell grid of
    Just (Box b) -> m {grid = insert selectedCell (boxToJunction b) grid}
    Just (Junction _) -> deleteCell m
    Nothing -> m

boxToJunction :: Box -> Cell
boxToJunction MkBox {up, down, left, right} =
  Junction $
    MkJunction (up /= None) (down /= None) (left /= None) (right /= None)

deleteCell :: Model -> Model
deleteCell = disconnectNeighbors . removeSelected
  where
    removeSelected m@Model {grid, selectedCell} = m {grid = delete selectedCell grid}
    disconnectNeighbors m@Model {grid, selectedCell = (x, y)} =
      m {grid = disconnectLeftNeighbor . disconnectRightNeighbor . disconnectUpNeighbor . disconnectDownNeighbor $ grid}
      where
        disconnectLeftNeighbor = disconnectRight (x - 1, y)
        disconnectRightNeighbor = disconnectLeft (x + 1, y)
        disconnectUpNeighbor = disconnectDown (x, y - 1)
        disconnectDownNeighbor = disconnectUp (x, y + 1)

disconnectLeft :: (Int, Int) -> Grid -> Grid
disconnectLeft = adjust f
  where
    f (Box b@(MkBox {})) = Box b {left = None}
    f (Junction j@(MkJunction {})) = Junction j {jLeft = False}

disconnectRight :: (Int, Int) -> Grid -> Grid
disconnectRight = adjust f
  where
    f (Box b@(MkBox {})) = Box b {right = None}
    f (Junction j@(MkJunction {})) = Junction j {jRight = False}

disconnectUp :: (Int, Int) -> Grid -> Grid
disconnectUp = adjust f
  where
    f (Box b@(MkBox {})) = Box b {up = None}
    f (Junction j@(MkJunction {})) = Junction j {jUp = False}

disconnectDown :: (Int, Int) -> Grid -> Grid
disconnectDown = adjust f
  where
    f (Box b@(MkBox {})) = Box b {down = None}
    f (Junction j@(MkJunction {})) = Junction j {jDown = False}

toRenderModel :: Model -> RenderModel
toRenderModel (Model grid (selX, selY)) =
  let renderCell ((x, y), c) = RenderCell c (x == selX && y == selY)
      getCell (x, y) = fromMaybe emptyCell (lookup (x, y) grid)
      cellAtSelection = findWithDefault emptyCell (selX, selY) grid
      gridWithSelection = insert (selX, selY) cellAtSelection grid
   in [ [ renderCell ((x, y), getCell (x, y))
        | y <- [minY gridWithSelection .. maxY gridWithSelection]
        ]
      | x <- [minX gridWithSelection .. maxX gridWithSelection]
      ]

emptyCell :: Cell
emptyCell = Junction $ MkJunction False False False False

appWidget :: RenderModel -> Widget ()
appWidget m =
  center $ hBox (map renderColumn m)

toWidget :: Int -> RenderCell -> Widget ()
toWidget colWidth (RenderCell {selected, cell = Box b}) = toBoxWidget colWidth selected b
toWidget _ (RenderCell {selected, cell = Junction j}) = toJunctionWidget selected j

toBoxWidget :: Int -> Bool -> Box -> Widget ()
toBoxWidget colWidth selected b =
  let content = label b
      boxWidget = withBorderStyle unicode . border . padAll 1 . str $ content
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
      withSelection =
        if selected
          then
            overrideAttr borderAttr selectedAttr
              . withAttr selectedAttr
          else id
   in withSelection $
        upConn
          <=> ( vCenter leftConn
                  <+> boxWidget
                  <+> vCenter rightConn
              )
          <=> downConn

toJunctionWidget :: Bool -> Junction -> Widget ()
toJunctionWidget selected j =
  let centerSymbol = str $ case (jUp j, jDown j, jLeft j, jRight j) of
        (False, False, False, False) -> " "
        (False, False, False, True) -> "o"
        (False, False, True, False) -> "o"
        (False, False, True, True) -> "─"
        (False, True, False, False) -> "o"
        (False, True, False, True) -> "╭"
        (False, True, True, False) -> "╮"
        (False, True, True, True) -> "┬"
        (True, False, False, False) -> "o"
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
      widget = topLine <=> (leftLine <+> centerSymbol <+> rightLine) <=> bottomLine
   in if selected
        then withAttr selectedAttr widget
        else widget

boxWidth :: String -> Int
boxWidth s = textWidth s + 6 -- contents + 2 * padding + 2 * border + 2 * border

columnWidth :: [Cell] -> Int
columnWidth column =
  let boxTexts [] = []
      boxTexts (c : cs) = case c of
        Box b -> label b : boxTexts cs
        _ -> boxTexts cs
   in maximum (6 : map boxWidth (boxTexts column))

renderColumn :: RenderColumn -> Widget ()
renderColumn column =
  Widget Fixed Fixed $ do
    let cw = columnWidth (map cell column)
    render $
      hLimit cw $
        vBox (map (vLimit 7 . hCenter . toWidget cw) column)
