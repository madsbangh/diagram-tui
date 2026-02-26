{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Control.Monad
import Data.Map hiding (map)
import Data.Maybe
import Graphics.Vty
import Prelude hiding (head, lookup)

data Model = Model
  { grid :: Grid
  , selectedCell :: CellCoord
  , currentMode :: EditorMode
  , clipboard :: Maybe Cell
  , undo :: Maybe Model
  , redo :: Maybe Model
  }

data Connections = Connections
  { up :: Connection
  , down :: Connection
  , left :: Connection
  , right :: Connection
  }

data EditorMode = Normal | InsertText | PendingDelete

type Grid = Map (Int, Int) Cell

type CellCoord = (Int, Int)

data Cell
  = Box String Connections
  | Label String Connections
  | Junction Connections

data Connection = None | Line | ArrowIn deriving (Eq)

data RenderModel = RenderModel
  { insertMode :: Bool
  , renderColumns :: [RenderColumn]
  }

type RenderColumn = [RenderCell]

data RenderCell = RenderCell {cell :: Cell, selected :: Bool}

main :: IO ()
main =
  void $
    defaultMain
      app
      ( addJunction U
          . addJunction L
          . setText "No"
          . addLabelHere
          . addJunction L
          . addJunction D
          . moveSelection L
          . moveSelection L
          . setText "End"
          . addBoxHere
          . addJunction R
          . setText "Yes"
          . addLabelHere
          . addJunction R
          . setText "Done?"
          . addBoxHere
          . addJunction R
          . addJunction R
          . setText "Process"
          . addBoxHere
          . addJunction R
          . setText "Start"
          . addLabelHere
          $ defaultModel
      )

defaultModel :: Model
defaultModel =
  Model
    { grid = empty
    , selectedCell = (0, 0)
    , currentMode = Normal
    , clipboard = Nothing
    , undo = Nothing
    , redo = Nothing
    }

app :: App Model e ()
app =
  App
    { appDraw = drawApp
    , appStartEvent = return ()
    , appHandleEvent = updateApp
    , appAttrMap =
        const
          ( attrMap
              defAttr
              [ (selectedAttr, bg (RGBColor 0 60 120))
              , (editedAttr, bg (RGBColor 120 120 60))
              , (sampleTextAttr, fg (RGBColor 128 128 128))
              , (editedTextAttr, fg (RGBColor 255 255 128))
              , (helpAttr, fg (RGBColor 140 140 140))
              ]
          )
    , appChooseCursor = neverShowCursor
    }

editedAttr :: AttrName
editedAttr = attrName "edited"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

helpAttr :: AttrName
helpAttr = attrName "helpAttr"

sampleTextAttr :: AttrName
sampleTextAttr = attrName "sampleText"

editedTextAttr :: AttrName
editedTextAttr = attrName "editedText"

drawApp :: Model -> [Widget ()]
drawApp m = [appWidget $ toRenderModel m, helpWidget]

helpWidget :: Widget ()
helpWidget =
  overrideAttr borderAttr helpAttr
    . withAttr helpAttr
    . padLeft Max
    . padTop Max
    . borderWithLabel (str "Help")
    . padAll 1
    . str
    $ "Some help text here"

recordUndo :: Model -> Model
recordUndo m = m{undo = Just m, redo = Nothing}

performUndo :: Model -> Model
performUndo m@Model{undo} = case undo of
  Just prevModel -> prevModel{redo = Just m}
  Nothing -> m

performRedo :: Model -> Model
performRedo m@Model{redo} = fromMaybe m redo

modifyWithUndo :: (Model -> Model) -> EventM () Model ()
modifyWithUndo f = modify $ f . recordUndo

updateApp :: BrickEvent () e -> EventM () Model ()
updateApp (VtyEvent (EvKey key [])) = do
  mode <- currentMode <$> get
  case mode of
    Normal ->
      case key of
        (KChar 'h') -> modify (moveSelection L)
        (KChar 'l') -> modify (moveSelection R)
        (KChar 'k') -> modify (moveSelection U)
        (KChar 'j') -> modify (moveSelection D)
        (KChar 'd') -> modify (toMode PendingDelete)
        (KChar 'x') -> modifyWithUndo deleteSelected
        (KChar 'y') -> modifyWithUndo yankSelected
        (KChar 'i') -> modifyWithUndo (addJunction L)
        (KChar 'a') -> modifyWithUndo (addJunction R)
        (KChar 'O') -> modifyWithUndo (addJunction U)
        (KChar 'o') -> modifyWithUndo (addJunction D)
        (KChar 'c') -> modifyWithUndo changeSelected
        (KChar 'r') -> modifyWithUndo replaceSelected
        (KChar 'b') -> modifyWithUndo (toMode InsertText . addBoxHere)
        (KChar 't') -> modifyWithUndo (toMode InsertText . addLabelHere)
        (KChar 'p') -> modifyWithUndo paste
        (KChar 'u') -> modify performUndo
        (KChar 'q') -> halt
        _ -> return ()
    InsertText ->
      case key of
        KEsc -> modify (toMode Normal)
        KEnter -> modify (toMode Normal)
        _ -> do
          m <- get
          let t = case getText m of
                (Just t') -> t'
                Nothing -> ""
          let editorState = editor () Nothing t
          (newEditorState, ()) <-
            nestEventM editorState $
              handleEditorEvent (VtyEvent (EvKey key []))
          let newText = unwords $ getEditContents newEditorState
          modify $ setText newText
    PendingDelete ->
      case key of
        KEsc -> modify (toMode Normal)
        (KChar 'd') -> modifyWithUndo (toMode Normal . deleteSelected)
        (KChar 'h') -> modifyWithUndo (toMode Normal . deleteConnection L)
        (KChar 'j') -> modifyWithUndo (toMode Normal . deleteConnection D)
        (KChar 'k') -> modifyWithUndo (toMode Normal . deleteConnection U)
        (KChar 'l') -> modifyWithUndo (toMode Normal . deleteConnection R)
        _ -> return ()
updateApp (VtyEvent (EvKey (KChar 'r') [MCtrl])) = modify performRedo
updateApp _ = return ()

paste :: Model -> Model
paste m@Model{clipboard} =
  case clipboard of
    Just (Junction cs) -> connectNeighborsToSelection . insertCell (Junction cs) $ m
    Just (Box s _) -> setText s . addBoxHere $ m
    Just (Label s _) -> setText s . addLabelHere $ m
    Nothing -> m

connectNeighborsToSelection :: Model -> Model
connectNeighborsToSelection m@Model{grid, selectedCell} =
  let connectNeighbor thisConnection dir =
        moveSelection (opposite dir)
          . connect (connector thisConnection) (opposite dir)
          . moveSelection dir
   in case connections <$> lookup selectedCell grid of
        Just cs ->
          connectNeighbor (left cs) L
            . connectNeighbor (right cs) R
            . connectNeighbor (up cs) U
            . connectNeighbor (down cs) D
            $ m
        Nothing -> m

-- Utility function to update the selected cell
updateSelected :: (Cell -> Maybe Cell) -> Model -> Model
updateSelected f m@Model{grid, selectedCell} =
  m{grid = Data.Map.update f selectedCell grid}

-- Utility function to alter the selected cell
alterSelected :: (Maybe Cell -> Maybe Cell) -> Model -> Model
alterSelected f m@Model{grid, selectedCell} =
  m{grid = alter f selectedCell grid}

deleteConnection :: Dir -> Model -> Model
deleteConnection dir =
  disconnectSelected (opposite dir)
    . moveSelection dir
    . disconnectSelected dir

disconnectSelected :: Dir -> Model -> Model
disconnectSelected dir m@Model{grid, selectedCell} =
  m{grid = disconnect dir selectedCell grid}

changeSelected :: Model -> Model
changeSelected m =
  if selectedCellHasText m
    then toMode InsertText m
    else m

replaceSelected :: Model -> Model
replaceSelected m =
  if selectedCellHasText m
    then toMode InsertText . setText mempty $ m
    else m

selectedCellHasText :: Model -> Bool
selectedCellHasText Model{grid, selectedCell} =
  maybe False hasText (lookup selectedCell grid)

hasText :: Cell -> Bool
hasText (Box _ _) = True
hasText (Label _ _) = True
hasText (Junction _) = False

getText :: Model -> Maybe String
getText Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just (Box s _) -> Just s
    Just (Label s _) -> Just s
    _ -> Nothing

setText :: String -> Model -> Model
setText t m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just (Box _ c) -> m{grid = insert selectedCell (Box t c) grid}
    Just (Label _ c) -> m{grid = insert selectedCell (Label t c) grid}
    _ -> m

addJunction :: Dir -> Model -> Model
addJunction dir = connectFrom (opposite dir) . moveSelection dir . connectTo dir

moveSelection :: Dir -> Model -> Model
moveSelection dir m@Model{selectedCell} =
  m{selectedCell = moveCoord dir selectedCell}

moveCoord :: Dir -> CellCoord -> CellCoord
moveCoord L (x, y) = (x - 1, y)
moveCoord R (x, y) = (x + 1, y)
moveCoord U (x, y) = (x, y - 1)
moveCoord D (x, y) = (x, y + 1)

connections :: Cell -> Connections
connections (Box _ cs) = cs
connections (Label _ cs) = cs
connections (Junction cs) = cs

connection :: Dir -> Connections -> Connection
connection L = left
connection R = right
connection U = up
connection D = down

mapConnections :: (Connections -> Connections) -> Cell -> Maybe Cell
mapConnections f (Box s cs) = Just $ Box s (f cs)
mapConnections f (Label s cs) = Just $ Label s (f cs)
mapConnections f (Junction cs) = case f cs of
  Connections None None None None -> Nothing
  cs' -> Just (Junction cs')

withConnection :: Dir -> Connection -> Connections -> Connections
withConnection L c cs = cs{left = c}
withConnection R c cs = cs{right = c}
withConnection U c cs = cs{up = c}
withConnection D c cs = cs{down = c}

disconnected :: Connections
disconnected = Connections None None None None

mkLabel :: Cell
mkLabel = Label mempty disconnected

mkBox :: Cell
mkBox = Box mempty disconnected

opposite :: Dir -> Dir
opposite L = R
opposite R = L
opposite U = D
opposite D = U

toMode :: EditorMode -> Model -> Model
toMode mode model = model{currentMode = mode}

addBoxHere :: Model -> Model
addBoxHere m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Nothing ->
      m
        { grid = insert selectedCell mkBox grid
        }
    Just (Junction _) -> junctionToBox m
    Just (Label _ _) -> labelToBox m
    _ -> m

addLabelHere :: Model -> Model
addLabelHere m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Nothing ->
      m
        { grid = insert selectedCell mkLabel grid
        }
    Just (Junction _) -> junctionToLabel m
    Just (Box _ _) -> boxToLabel m
    _ -> m

labelToBox :: Model -> Model
labelToBox = updateSelected $ \case
  Label s cs -> Just (Box s cs)
  c -> Just c

boxToLabel :: Model -> Model
boxToLabel = updateSelected $ \case
  Box s cs -> Just (Label s cs)
  c -> Just c

insertCell :: Cell -> Model -> Model
insertCell c m@Model{grid, selectedCell} = m{grid = insert selectedCell c grid}

data Dir = L | R | U | D deriving (Eq)

connectTo :: Dir -> Model -> Model
connectTo = connect Line

connectFrom :: Dir -> Model -> Model
connectFrom = connect ArrowIn

connect :: Connection -> Dir -> Model -> Model
connect conn dir =
  alterSelected $
    \case
      Just c ->
        mapConnections (withConnection dir conn) c
      Nothing ->
        mapConnections (withConnection dir conn) emptyCell

getNeighboringConnection :: Dir -> Model -> Connection
getNeighboringConnection dir Model{grid, selectedCell} =
  case lookup (moveCoord dir selectedCell) grid of
    Just c -> connection (opposite dir) (connections c)
    _ -> None

connector :: Connection -> Connection
connector None = None
connector ArrowIn = Line
connector Line = ArrowIn

connectToNeighbors :: Model -> Model
connectToNeighbors m@Model{grid, selectedCell} =
  let cs =
        Connections
          { left = connector (getNeighboringConnection L m)
          , right = connector (getNeighboringConnection R m)
          , up = connector (getNeighboringConnection U m)
          , down = connector (getNeighboringConnection D m)
          }
   in case lookup selectedCell grid of
        Just (Box s _) -> insertCell (Box s cs) m
        Just (Label s _) -> insertCell (Label s cs) m
        _ ->
          case cs of
            (Connections None None None None) -> m
            _ -> insertCell (Junction cs) m

junctionToBox :: Model -> Model
junctionToBox m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just Junction{} ->
      connectToNeighbors . insertCell mkBox $ m
    _ -> m

junctionToLabel :: Model -> Model
junctionToLabel m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just Junction{} ->
      let m' = m{grid = insert selectedCell mkLabel grid}
       in connectToNeighbors m'
    _ -> m

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
deleteSelected m@Model{grid, selectedCell} =
  case Data.Map.lookup selectedCell grid of
    Just
      (Box _ Connections{left = None, right = None, up = None, down = None}) ->
        (yankSelected m){grid = delete selectedCell grid}
    Just (Box _ cs) -> (yankSelected m){grid = insert selectedCell (Junction cs) grid}
    Just (Label _ cs) -> (yankSelected m){grid = insert selectedCell (Junction cs) grid}
    Just (Junction _) -> deleteCell . yankSelected $ m
    Nothing -> m

yankSelected :: Model -> Model
yankSelected m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just c -> m{clipboard = Just c}
    _ -> m

deleteCell :: Model -> Model
deleteCell = disconnectNeighbors . removeSelected
 where
  removeSelected m@Model{grid, selectedCell} = m{grid = delete selectedCell grid}
  disconnectNeighbor dir pos = disconnect dir (moveCoord (opposite dir) pos)
  disconnectNeighbors m@Model{grid, selectedCell} =
    m
      { grid =
          disconnectNeighbor L selectedCell
            . disconnectNeighbor R selectedCell
            . disconnectNeighbor U selectedCell
            . disconnectNeighbor D selectedCell
            $ grid
      }

disconnect :: Dir -> CellCoord -> Grid -> Grid
disconnect dir = Data.Map.update $ mapConnections (withConnection dir None)

toRenderModel :: Model -> RenderModel
toRenderModel (Model grid (selX, selY) mode _ _ _) =
  let renderCell ((x, y), c) = RenderCell c (x == selX && y == selY)
      getCellOrEmpty (x, y) = fromMaybe emptyCell (lookup (x, y) grid)
      cellAtSelection = findWithDefault emptyCell (selX, selY) grid
      gridWithSelection = insert (selX, selY) cellAtSelection grid
   in RenderModel
        (case mode of InsertText -> True; _ -> False)
        [ [ renderCell ((x, y), getCellOrEmpty (x, y))
          | y <- [minY gridWithSelection .. maxY gridWithSelection]
          ]
        | x <- [minX gridWithSelection .. maxX gridWithSelection]
        ]

emptyCell :: Cell
emptyCell = Junction disconnected

appWidget :: RenderModel -> Widget ()
appWidget RenderModel{insertMode, renderColumns} =
  hBox (map (renderColumn insertMode) renderColumns)

toWidget :: Bool -> Int -> RenderCell -> Widget ()
toWidget insertMode colWidth (RenderCell{selected, cell = Box s cs}) = toBoxWidget colWidth selected insertMode s cs
toWidget insertMode colWidth (RenderCell{selected, cell = Label s cs}) = toLabelWidget colWidth selected insertMode s cs
toWidget _ _ (RenderCell{selected, cell = Junction cs}) = toJunctionWidget selected cs

sampleText :: [Char]
sampleText = "Insert text..."

sampleTextStyle :: Widget n -> Widget n
sampleTextStyle = withAttr sampleTextAttr

editedTextStyle :: Widget n -> Widget n
editedTextStyle = withAttr editedTextAttr

toBoxWidget :: Int -> Bool -> Bool -> String -> Connections -> Widget ()
toBoxWidget colWidth selected insertMode s cs =
  let (contentStyle, content) = case s of
        "" -> (sampleTextStyle, sampleText)
        _ -> (if selected && insertMode then editedTextStyle else id, s)
      boxWidget =
        withBorderStyle unicode
          . border
          . padAll 1
          . contentStyle
          . str
          $ content
      extraWidth = colWidth - boxWidth content
      upConn = case up cs of
        None -> str $ replicate colWidth ' '
        Line -> hCenter (str "│")
        ArrowIn -> hCenter (str "▼")
      downConn = case down cs of
        None -> str $ replicate colWidth ' '
        Line -> hCenter (str "│")
        ArrowIn -> hCenter (str "▲")
      leftConn = str $ case left cs of
        None -> spaces
        Line -> hLineL ++ "─"
        ArrowIn -> hLineL ++ "►"
      rightConn = case right cs of
        None -> emptyWidget
        Line -> str "─" <+> hLineR
        ArrowIn -> str "◄" <+> hLineR
      hLineL = replicate (extraWidth `div` 2) '─'
      hLineR = vLimit 1 (fill '─')
      spaces = replicate (extraWidth `div` 2 + 1) ' '
      selEdAttr = if insertMode then editedAttr else selectedAttr
      withSelection =
        if selected
          then
            overrideAttr borderAttr selEdAttr
              . withAttr selEdAttr
          else id
   in withSelection $
        upConn
          <=> ( vCenter leftConn
                  <+> boxWidget
                  <+> vCenter rightConn
              )
          <=> downConn

toJunctionWidget :: Bool -> Connections -> Widget ()
toJunctionWidget selected j =
  let centerSymbol = str $ case (up j /= None, down j /= None, left j /= None, right j /= None) of
        (False, False, False, False) -> " "
        (False, False, False, True) -> " "
        (False, False, True, False) -> " "
        (False, False, True, True) -> "─"
        (False, True, False, False) -> " "
        (False, True, False, True) -> "╭"
        (False, True, True, False) -> "╮"
        (False, True, True, True) -> "┬"
        (True, False, False, False) -> " "
        (True, False, False, True) -> "╰"
        (True, False, True, False) -> "╯"
        (True, False, True, True) -> "┴"
        (True, True, False, False) -> "│"
        (True, True, False, True) -> "├"
        (True, True, True, False) -> "┤"
        (True, True, True, True) -> "┼"
      vLine c = hCenter $ hLimit 1 $ vLimit 3 $ fill c
      topLine =
        if up j /= None
          then vLine '│'
          else vLine ' '
      bottomLine =
        if down j /= None
          then vLine '│'
          else vLine ' '
      leftLine =
        if left j /= None
          then fill '─'
          else fill ' '
      rightLine =
        if right j /= None
          then fill '─'
          else fill ' '
      widget =
        topLine
          <=> (leftLine <+> centerSymbol <+> rightLine)
          <=> bottomLine
   in if selected
        then withAttr selectedAttr widget
        else widget

toLabelWidget :: Int -> Bool -> Bool -> String -> Connections -> Widget ()
toLabelWidget colWidth selected insertMode s cs =
  let vLine c = hCenter $ hLimit 1 $ vLimit 3 $ fill c
      topLine =
        if up cs /= None
          then vLine '│'
          else vLine ' '
      bottomLine =
        if down cs /= None
          then vLine '│'
          else vLine ' '
      leftLine =
        if left cs /= None
          then fill '─'
          else fill ' '
      rightLine =
        str $
          replicate rightLineWidth $
            if right cs /= None
              then '─'
              else ' '
      contentWidth = textWidth content
      remainingWidth = colWidth - contentWidth
      rightLineWidth = remainingWidth `div` 2
      label = contentStyle . str $ content
      widget = topLine <=> (leftLine <+> label <+> rightLine) <=> bottomLine
      selEdAttr = if insertMode then editedAttr else selectedAttr
      (contentStyle, content) = case s of
        "" -> (sampleTextStyle, sampleText)
        _ -> (if selected && insertMode then editedTextStyle else id, s)
   in if selected
        then withAttr selEdAttr widget
        else widget

boxWidth :: String -> Int
boxWidth s = textWidth s + 6 -- contents + 2 * padding + 2 * border + 2 * border

columnWidth :: [Cell] -> Int
columnWidth column =
  let texts [] = []
      texts (c : cs) = case c of
        Box s _ ->
          let content = case s of
                "" -> sampleText
                _ -> s
           in content : texts cs
        Label s _ ->
          let content = case s of
                "" -> sampleText
                _ -> s
           in content : texts cs
        _ -> texts cs
      w = maximum (6 : map boxWidth (texts column))
   in if even w then w + 1 else w

renderColumn :: Bool -> RenderColumn -> Widget ()
renderColumn insertMode column =
  Widget Fixed Fixed $ do
    let cw = columnWidth (map cell column)
    render $
      hLimit cw $
        vBox (map (vLimit 7 . hCenter . toWidget insertMode cw) column)
