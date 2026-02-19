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
  }

data EditorMode = Normal | PendingInsert Dir | InsertText | InsertLine

type Grid = Map (Int, Int) Cell

type CellCoord = (Int, Int)

data Cell = Box Box | Junction Junction

data Box = MkBox
  { label :: String
  , up :: Connection
  , down :: Connection
  , left :: Connection
  , right :: Connection
  }

data Junction = MkJunction
  { jUp :: Bool
  , jDown :: Bool
  , jLeft :: Bool
  , jRight :: Bool
  }

data Connection = None | Line | ArrowIn deriving (Eq)

type RenderModel = [RenderColumn]

type RenderColumn = [RenderCell]

data RenderCell = RenderCell {cell :: Cell, selected :: Bool}

main :: IO ()
main =
  void $
    defaultMain
      app
      ( setText "End"
          . addBox R
          . addJunction R
          . setText "Start"
          . addBoxHere
          $ Model
            { grid =
                empty
            , selectedCell = (0, 0)
            , currentMode = Normal
            }
      )

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
              ]
          )
    , appChooseCursor = neverShowCursor
    }

editedAttr :: AttrName
editedAttr = attrName "edited"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

sampleTextAttr :: AttrName
sampleTextAttr = attrName "sampleText"

editedTextAttr :: AttrName
editedTextAttr = attrName "editedText"

drawApp :: Model -> [Widget ()]
drawApp m = [appWidget (isInsertTextMode m || isInsertLineMode m) $ toRenderModel m]

isPendingInsertMode :: Model -> Bool
isPendingInsertMode Model{currentMode = PendingInsert _} = True
isPendingInsertMode _ = False

isInsertTextMode :: Model -> Bool
isInsertTextMode Model{currentMode = InsertText} = True
isInsertTextMode _ = False

isInsertLineMode :: Model -> Bool
isInsertLineMode Model{currentMode = InsertLine} = True
isInsertLineMode _ = False

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
        (KChar 'd') -> modify deleteSelected
        (KChar 'x') -> modify deleteSelected
        (KChar 'i') -> modify (toMode $ PendingInsert L)
        (KChar 'a') -> modify (toMode $ PendingInsert R)
        (KChar 'O') -> modify (toMode $ PendingInsert U)
        (KChar 'o') -> modify (toMode $ PendingInsert D)
        (KChar 'c') -> modify (toMode InsertLine)
        (KChar 'r') -> modify (connectTo R)
        (KChar 'q') -> halt
        _ -> return ()
    PendingInsert dir ->
      case key of
        (KChar 'b') -> modify (toMode InsertText . addBox dir)
        (KChar 'l') -> modify (toMode InsertLine . addJunction dir)
        _ -> return ()
    InsertLine ->
      case key of
        (KChar 'h') -> modify (addJunction L)
        (KChar 'l') -> modify (addJunction R)
        (KChar 'k') -> modify (addJunction U)
        (KChar 'j') -> modify (addJunction D)
        (KChar 'b') -> modify (toMode InsertText . addBoxHere)
        (KChar 'i') -> modify (toMode $ PendingInsert L)
        (KChar 'a') -> modify (toMode $ PendingInsert R)
        (KChar 'O') -> modify (toMode $ PendingInsert U)
        (KChar 'o') -> modify (toMode $ PendingInsert D)
        KEsc -> modify (toMode Normal)
        _ -> return ()
    InsertText ->
      case key of
        KEsc -> modify (toMode Normal)
        KEnter -> modify (toMode InsertLine)
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
updateApp _ = return ()

getText :: Model -> Maybe String
getText Model{grid, selectedCell} = do
  (Box b) <- lookup selectedCell grid
  return (label b)

setText :: String -> Model -> Model
setText t m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just (Box b) -> m{grid = insert selectedCell (Box b{label = t}) grid}
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

withConnection :: Connection -> Dir -> Box -> Box
withConnection c L b = b{left = c}
withConnection c R b = b{right = c}
withConnection c U b = b{up = c}
withConnection c D b = b{down = c}

mkBox :: Box
mkBox = MkBox mempty None None None None

opposite :: Dir -> Dir
opposite L = R
opposite R = L
opposite U = D
opposite D = U

addBox :: Dir -> Model -> Model
addBox dir m@Model{grid, selectedCell = (x, y)} =
  let coords = moveCoord dir (x, y)
   in case lookup coords grid of
        Nothing ->
          let m'@Model{grid = grid'} = connectTo dir m
           in m'
                { grid =
                    insert
                      coords
                      (Box $ withConnection ArrowIn (opposite dir) mkBox)
                      grid'
                , selectedCell = coords
                }
        Just (Junction _) ->
          let m'@Model{grid = grid'} = connectTo dir m
              m'' =
                m'
                  { grid = insert coords (Box mkBox) grid'
                  , selectedCell = coords
                  }
           in connectToNeighbors m''
        _ ->
          connectToNeighbors
            . addBox dir
            . makeSpace dir
            . connectTo dir
            $ m

toMode :: EditorMode -> Model -> Model
toMode mode model = model{currentMode = mode}

addBoxHere :: Model -> Model
addBoxHere m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Nothing ->
      m
        { grid = insert selectedCell (Box mkBox) grid
        }
    Just (Junction _) -> junctionToBox m
    _ -> setText mempty m

data Dir = L | R | U | D deriving (Eq)

connectTo :: Dir -> Model -> Model
connectTo = connect Line

connectFrom :: Dir -> Model -> Model
connectFrom = connect ArrowIn

connect :: Connection -> Dir -> Model -> Model
connect conn dir m@Model{grid, selectedCell} = m{grid = alter f selectedCell grid}
 where
  f c = case c of
    Just (Box b) -> Just $ case dir of
      L -> Box $ b{left = conn}
      R -> Box $ b{right = conn}
      U -> Box $ b{up = conn}
      D -> Box $ b{down = conn}
    Just (Junction j) -> Just $ case dir of
      L -> Junction $ j{jLeft = True}
      R -> Junction $ j{jRight = True}
      U -> Junction $ j{jUp = True}
      D -> Junction $ j{jDown = True}
    Nothing -> Just $ case dir of
      L -> Junction $ emptyJunction{jLeft = True}
      R -> Junction $ emptyJunction{jRight = True}
      U -> Junction $ emptyJunction{jUp = True}
      D -> Junction $ emptyJunction{jDown = True}

getNeighboringConnection :: Dir -> Model -> Connection
getNeighboringConnection dir Model{grid, selectedCell} =
  case lookup (moveCoord dir selectedCell) grid of
    Just (Box (MkBox{right})) | dir == L -> right
    Just (Box (MkBox{left})) | dir == R -> left
    Just (Box (MkBox{down})) | dir == U -> down
    Just (Box (MkBox{up})) | dir == D -> up
    Just (Junction (MkJunction{jRight = True})) | dir == L -> Line
    Just (Junction (MkJunction{jLeft = True})) | dir == R -> Line
    Just (Junction (MkJunction{jDown = True})) | dir == U -> Line
    Just (Junction (MkJunction{jUp = True})) | dir == D -> Line
    _ -> None

connectToNeighbors :: Model -> Model
connectToNeighbors m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just (Box b@MkBox{left, right, up, down}) ->
      let
        connector _ None = None
        connector _ ArrowIn = Line
        connector None Line = ArrowIn
        connector existing Line = existing
       in
        m
          { grid =
              insert
                selectedCell
                ( Box
                    b
                      { left = connector left (getNeighboringConnection L m)
                      , right = connector right (getNeighboringConnection R m)
                      , up = connector up (getNeighboringConnection U m)
                      , down = connector down (getNeighboringConnection D m)
                      }
                )
                grid
          }
    _ ->
      let j' =
            MkJunction
              { jLeft = connector (getNeighboringConnection L m)
              , jRight = connector (getNeighboringConnection R m)
              , jUp = connector (getNeighboringConnection U m)
              , jDown = connector (getNeighboringConnection D m)
              }
          connector None = False
          connector _ = True
       in case isEmptyJunciton j' of
            False -> m{grid = insert selectedCell (Junction j') grid}
            True -> m

isEmptyJunciton :: Junction -> Bool
isEmptyJunciton (MkJunction False False False False) = True
isEmptyJunciton _ = False

isCoordPerpendicular :: Dir -> CellCoord -> CellCoord -> Bool
isCoordPerpendicular U (_, selY) (_, y) = selY == y
isCoordPerpendicular D (_, selY) (_, y) = selY == y
isCoordPerpendicular L (selX, _) (x, _) = selX == x
isCoordPerpendicular R (selX, _) (x, _) = selX == x

isCoordOnSide :: Dir -> CellCoord -> CellCoord -> Bool
isCoordOnSide U (_, selY) (_, y) = selY > y
isCoordOnSide D (_, selY) (_, y) = selY < y
isCoordOnSide L (selX, _) (x, _) = selX > x
isCoordOnSide R (selX, _) (x, _) = selX < x

makeSpace :: Dir -> Model -> Model
makeSpace dir m@Model{grid, selectedCell = sel} = m{grid = mapKeys f grid}
 where
  f orig | isCoordOnSide dir sel orig = moveCoord dir orig
  f orig = orig

data Orientation = V | H

fillHoles :: Orientation -> Model -> Model
fillHoles o m@Model{grid, selectedCell} =
  let coords = case o of
        V -> undefined
        H -> undefined
   in select selectedCell . undefined $ m

select :: CellCoord -> Model -> Model
select coord m = m{selectedCell = coord}

getCell :: Model -> Maybe Cell
getCell Model{grid, selectedCell} = lookup selectedCell grid

junctionToBox :: Model -> Model
junctionToBox m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just Junction{} ->
      let m' = m{grid = insert selectedCell (Box mkBox) grid}
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
      (Box MkBox{left = None, right = None, up = None, down = None}) ->
        m{grid = delete selectedCell grid}
    Just (Box b) -> m{grid = insert selectedCell (boxToJunction b) grid}
    Just (Junction _) -> deleteCell m
    Nothing -> m

boxToJunction :: Box -> Cell
boxToJunction MkBox{up, down, left, right} =
  Junction $
    MkJunction (up /= None) (down /= None) (left /= None) (right /= None)

deleteCell :: Model -> Model
deleteCell = disconnectNeighbors . removeSelected
 where
  removeSelected m@Model{grid} | size grid == 1 = m
  removeSelected m@Model{grid, selectedCell} = m{grid = delete selectedCell grid}
  disconnectNeighbors m@Model{grid, selectedCell = (x, y)} =
    m{grid = disconnectLeftNeighbor . disconnectRightNeighbor . disconnectUpNeighbor . disconnectDownNeighbor $ grid}
   where
    disconnectLeftNeighbor = disconnectRight (x - 1, y)
    disconnectRightNeighbor = disconnectLeft (x + 1, y)
    disconnectUpNeighbor = disconnectDown (x, y - 1)
    disconnectDownNeighbor = disconnectUp (x, y + 1)

disconnectLeft :: (Int, Int) -> Grid -> Grid
disconnectLeft = adjust f
 where
  f (Box b@(MkBox{})) = Box b{left = None}
  f (Junction j@(MkJunction{})) = Junction j{jLeft = False}

disconnectRight :: (Int, Int) -> Grid -> Grid
disconnectRight = adjust f
 where
  f (Box b@(MkBox{})) = Box b{right = None}
  f (Junction j@(MkJunction{})) = Junction j{jRight = False}

disconnectUp :: (Int, Int) -> Grid -> Grid
disconnectUp = adjust f
 where
  f (Box b@(MkBox{})) = Box b{up = None}
  f (Junction j@(MkJunction{})) = Junction j{jUp = False}

disconnectDown :: (Int, Int) -> Grid -> Grid
disconnectDown = adjust f
 where
  f (Box b@(MkBox{})) = Box b{down = None}
  f (Junction j@(MkJunction{})) = Junction j{jDown = False}

toRenderModel :: Model -> RenderModel
toRenderModel (Model grid (selX, selY) _) =
  let renderCell ((x, y), c) = RenderCell c (x == selX && y == selY)
      getCellOrEmpty (x, y) = fromMaybe emptyCell (lookup (x, y) grid)
      cellAtSelection = findWithDefault emptyCell (selX, selY) grid
      gridWithSelection = insert (selX, selY) cellAtSelection grid
   in [ [ renderCell ((x, y), getCellOrEmpty (x, y))
        | y <- [minY gridWithSelection .. maxY gridWithSelection]
        ]
      | x <- [minX gridWithSelection .. maxX gridWithSelection]
      ]

emptyCell :: Cell
emptyCell = Junction $ emptyJunction

emptyJunction :: Junction
emptyJunction = MkJunction False False False False

appWidget :: Bool -> RenderModel -> Widget ()
appWidget insertMode m =
  hBox (map (renderColumn insertMode) m)

toWidget :: Bool -> Int -> RenderCell -> Widget ()
toWidget insertMode colWidth (RenderCell{selected, cell = Box b}) = toBoxWidget colWidth selected insertMode b
toWidget _ _ (RenderCell{selected, cell = Junction j}) = toJunctionWidget selected j

sampleText :: [Char]
sampleText = "Insert text..."

toBoxWidget :: Int -> Bool -> Bool -> Box -> Widget ()
toBoxWidget colWidth selected insertMode b =
  let (contentStyle, content) = case label b of
        "" -> (sampleTextStyle, sampleText)
        s -> (if selected && insertMode then editedTextStyle else id, s)
      boxWidget = withBorderStyle unicode . border . padAll 1 . contentStyle . str $ content
      extraWidth = colWidth - boxWidth content
      sampleTextStyle = withAttr sampleTextAttr
      editedTextStyle = withAttr editedTextAttr
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

toJunctionWidget :: Bool -> Junction -> Widget ()
toJunctionWidget selected j =
  let centerSymbol = str $ case (jUp j, jDown j, jLeft j, jRight j) of
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
        Box b ->
          let content = case label b of
                "" -> sampleText
                s -> s
           in content : boxTexts cs
        _ -> boxTexts cs
      w = maximum (6 : map boxWidth (boxTexts column))
   in if even w then w + 1 else w

renderColumn :: Bool -> RenderColumn -> Widget ()
renderColumn insertMode column =
  Widget Fixed Fixed $ do
    let cw = columnWidth (map cell column)
    render $
      hLimit cw $
        vBox (map (vLimit 7 . hCenter . toWidget insertMode cw) column)
