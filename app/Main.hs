{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad
import Data.List.NonEmpty (NonEmpty ((:|)), head, toList)
import Graphics.Vty
import Prelude hiding (head)

data Model = Model
  { columns :: Grid,
    selectedCell :: CellCoord
  }

type Grid = NonEmpty Column

type Column = NonEmpty Cell

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
      col1 = startBox :| [emptyCell]
      col2 = junction :| [emptyCell]
      col3 = endBox :| [bottomBox]
   in void $
        defaultMain
          app
          ( Model
              { columns = col1 :| [col2, col3],
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
moveSelectionLeft m@Model {selectedCell = (x, y)}
  | x > 0 = m {selectedCell = (x - 1, y)}
  | otherwise = m

moveSelectionRight :: Model -> Model
moveSelectionRight m@Model {columns, selectedCell = (x, y)}
  | x < length columns - 1 = m {selectedCell = (x + 1, y)}
  | otherwise = m

moveSelectionUp :: Model -> Model
moveSelectionUp m@Model {columns, selectedCell = (x, y)}
  | y > 0 = m {selectedCell = (x, y - 1)}
  | otherwise = m

moveSelectionDown :: Model -> Model
moveSelectionDown m@Model {columns, selectedCell = (x, y)}
  | y < (length . head $ columns) - 1 = m {selectedCell = (x, y + 1)}
  | otherwise = m

deleteSelected :: Model -> Model
deleteSelected = undefined

deleteBox :: Box -> Cell
deleteBox MkBox {up, down, left, right} =
  Junction $
    MkJunction (up /= None) (down /= None) (left /= None) (right /= None)

emptyCell :: Cell
emptyCell = Junction $ MkJunction False False False False

toRenderModel :: Model -> RenderModel
toRenderModel (Model cols (selX, selY)) =
  let colsWithCoords =
        [ [((x, y), cell) | (y, cell) <- zip [0 ..] (toList cells)]
        | (x, cells) <- zip [0 ..] (toList cols)
        ]
      renderCell ((x, y), c) = RenderCell c (x == selX && y == selY)
   in map (map renderCell) colsWithCoords

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
      withSelection = if selected then withAttr selectedAttr else id
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
