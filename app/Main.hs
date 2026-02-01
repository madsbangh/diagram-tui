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

type ModelColumn = [Cell]

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
      endBox = Box $ MkBox "Left" None None ArrowIn None
   in void $
        defaultMain
          app
          ( Model
              [[junction], [startBox]]
              (ModelSelectedColumn [] endBox [])
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
  (KChar 'h') -> modify $ moveSelection L
  (KChar 'l') -> modify $ moveSelection R
  (KChar 'k') -> modify $ moveSelection U
  (KChar 'j') -> modify $ moveSelection D
  (KChar 'q') -> halt
  KEsc -> halt
  _ -> return ()
updateApp _ = return ()

data MoveDirection = L | R | U | D

moveSelection :: MoveDirection -> Model -> Model
moveSelection L m@Model {columnsLeft, selectedColumn, columnsRight}
  | canMoveInto columnsLeft =
      Model
        (tail columnsLeft)
        (toSelectedColumn . head $ columnsLeft)
        (toColumn selectedColumn : columnsRight)
  | otherwise = m
moveSelection R m@Model {columnsLeft, selectedColumn, columnsRight}
  | canMoveInto columnsRight =
      Model
        (toColumn selectedColumn : columnsLeft)
        (toSelectedColumn . head $ columnsRight)
        (tail columnsRight)
  | otherwise = m
moveSelection U m@Model {selectedColumn} = m {selectedColumn = moveUp selectedColumn}
moveSelection D m@Model {selectedColumn} = m {selectedColumn = moveDown selectedColumn}

moveUp :: ModelSelectedColumn -> ModelSelectedColumn
moveUp = id

moveDown :: ModelSelectedColumn -> ModelSelectedColumn
moveDown = id

toSelectedColumn :: ModelColumn -> ModelSelectedColumn
toSelectedColumn column =
  ModelSelectedColumn
    []
    (head column)
    (tail column)

toColumn :: ModelSelectedColumn -> ModelColumn
toColumn ModelSelectedColumn {cellsAbove, selectedCell, cellsBelow} = reverse cellsAbove ++ [selectedCell] ++ cellsBelow

canMoveInto :: [ModelColumn] -> Bool
canMoveInto [] = False
canMoveInto (next : _) | null next = False
canMoveInto (_ : _) = True

toRenderModel :: Model -> RenderModel
toRenderModel (Model leftCols (ModelSelectedColumn {selectedCell = s}) rightCols) =
  let unselectedColumns = map (map (`RenderCell` False))
   in unselectedColumns (reverse leftCols)
        ++ [[RenderCell {selected = True, cell = s}]]
        ++ unselectedColumns rightCols

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
