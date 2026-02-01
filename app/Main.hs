{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad
import Graphics.Vty

data Model = Model
  { columnsLeft :: [ModelColumn],
    selectedColumn :: ModelSelectedColumn,
    columnsRight :: [ModelColumn]
  }

data ModelColumn = ModelColumn Cell [Cell]

toList :: ModelColumn -> [Cell]
toList (ModelColumn h t) = h : t

data ModelSelectedColumn = ModelSelectedColumn
  { cellsAbove :: [Cell],
    selectedCell :: Cell,
    cellsBelow :: [Cell]
  }

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

data Connection = None | Line | ArrowIn

type RenderModel = [RenderColumn]

type RenderColumn = [RenderCell]

data RenderCell = RenderCell {cell :: Cell, selected :: Bool}

main :: IO ()
main =
  let startBox = Box $ MkBox "Start" None None None Line
      junction = Junction $ MkJunction False False True True
      endBox = Box $ MkBox "End" None Line ArrowIn None
      bottomBox = Box $ MkBox "Another box" ArrowIn None None None
   in void $
        defaultMain
          app
          ( Model
              [ModelColumn junction [], ModelColumn startBox []]
              (ModelSelectedColumn [] endBox [bottomBox])
              []
          )

app :: App Model e ()
app =
  App
    { appDraw = drawApp,
      appStartEvent = return (),
      appHandleEvent = updateApp,
      appAttrMap = const (attrMap defAttr [(selectedAttr, black `on` red)]),
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
  (KChar 'q') -> halt
  KEsc -> halt
  _ -> return ()
updateApp _ = return ()

hFlip :: Model -> Model
hFlip Model {columnsLeft, selectedColumn, columnsRight} =
  Model columnsRight selectedColumn columnsLeft

vFlip :: Model -> Model
vFlip m =
  let flipped = ModelSelectedColumn newAbove mid newBelow
      newAbove = cellsBelow col
      mid = selectedCell col
      newBelow = cellsAbove col
      col = selectedColumn m
   in m {selectedColumn = flipped}

moveSelectionLeft :: Model -> Model
moveSelectionLeft m@Model {columnsLeft, selectedColumn, columnsRight} =
  case columnsLeft of
    (newSelection : reminaingColsLeft) ->
      Model
        { columnsLeft = reminaingColsLeft,
          selectedColumn = toSelectedColumn newSelection,
          columnsRight = toColumn selectedColumn : columnsRight
        }
    _ -> m

moveSelectionRight :: Model -> Model
moveSelectionRight = hFlip . moveSelectionLeft . hFlip

moveSelectionUp :: Model -> Model
moveSelectionUp m@Model {selectedColumn} = m {selectedColumn = moveUpInColumn selectedColumn}
  where
    moveUpInColumn ModelSelectedColumn {cellsAbove = (c : cs), selectedCell, cellsBelow} =
      ModelSelectedColumn
        { cellsAbove = cs,
          selectedCell = c,
          cellsBelow = selectedCell : cellsBelow
        }
    moveUpInColumn c = c

moveSelectionDown :: Model -> Model
moveSelectionDown = vFlip . moveSelectionUp . vFlip

toSelectedColumn :: ModelColumn -> ModelSelectedColumn
toSelectedColumn (ModelColumn h t) =
  ModelSelectedColumn [] h t

toColumn :: ModelSelectedColumn -> ModelColumn
toColumn ModelSelectedColumn {cellsAbove, selectedCell, cellsBelow} =
  case reverse cellsAbove of
    (h : remainingCellsAbove) -> ModelColumn h (remainingCellsAbove ++ [selectedCell] ++ cellsBelow)
    _ -> ModelColumn selectedCell cellsBelow

toRenderModel :: Model -> RenderModel
toRenderModel (Model leftCols selectedCol rightCols) =
  let renderUnselectedColumn = (map (`RenderCell` False) . toList)
      renderSelectedColumn (ModelSelectedColumn {cellsAbove, selectedCell, cellsBelow}) =
        (map (`RenderCell` False) . reverse $ cellsAbove)
          ++ [(RenderCell {cell = selectedCell, selected = True})]
          ++ map (`RenderCell` False) cellsBelow
   in map renderUnselectedColumn (reverse leftCols)
        ++ [renderSelectedColumn selectedCol]
        ++ map renderUnselectedColumn rightCols

appWidget :: RenderModel -> Widget ()
appWidget m =
  center $ hBox (map renderColumn m)

box :: Bool -> String -> Widget ()
box True = withAttr selectedAttr . withBorderStyle unicodeBold . border . padAll 1 . str
box False = withBorderStyle unicode . border . padAll 1 . str

toWidget :: Int -> RenderCell -> Widget ()
toWidget colWidth (RenderCell {selected, cell = Box b}) = toBoxWidget colWidth selected b
toWidget _ (RenderCell {selected, cell = Junction j}) = toJunctionWidget selected j

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
