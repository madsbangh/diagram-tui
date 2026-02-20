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

data EditorMode = Normal | InsertText | PendingDelete

type Grid = Map (Int, Int) Cell

type CellCoord = (Int, Int)

data Cell = Box Box | Junction Junction | Label Label

data Box = MkBox
  { bText :: String
  , bUp :: Connection
  , bDown :: Connection
  , bLeft :: Connection
  , bRight :: Connection
  }

data Junction = MkJunction
  { jUp :: Bool
  , jDown :: Bool
  , jLeft :: Bool
  , jRight :: Bool
  }

data Label = MkLabel
  { lText :: String
  , lUp :: Bool
  , lDown :: Bool
  , lLeft :: Bool
  , lRight :: Bool
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
drawApp m = [appWidget (isInsertTextMode m) $ toRenderModel m]

isInsertTextMode :: Model -> Bool
isInsertTextMode Model{currentMode = InsertText} = True
isInsertTextMode _ = False

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
        (KChar 'x') -> modify deleteSelected
        (KChar 'i') -> modify (addJunction L)
        (KChar 'a') -> modify (addJunction R)
        (KChar 'O') -> modify (addJunction U)
        (KChar 'o') -> modify (addJunction D)
        (KChar 'c') -> modify changeSelected
        (KChar 'b') -> modify (toMode InsertText . addBoxHere)
        (KChar 't') -> modify (toMode InsertText . addLabelHere)
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
        -- TODO
        _ -> return ()
updateApp _ = return ()

changeSelected :: Model -> Model
changeSelected m@Model{grid, selectedCell} = case lookup selectedCell grid of
  Just (Box{}) -> toMode InsertText . setText mempty $ m
  Just (Label{}) -> toMode InsertText . setText mempty $ m
  _ -> m

getText :: Model -> Maybe String
getText Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just (Box b) -> Just $ bText b
    Just (Label l) -> Just $ lText l
    _ -> Nothing

setText :: String -> Model -> Model
setText t m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just (Box b) -> m{grid = insert selectedCell (Box b{bText = t}) grid}
    Just (Label l) -> m{grid = insert selectedCell (Label l{lText = t}) grid}
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
withConnection c L b = b{bLeft = c}
withConnection c R b = b{bRight = c}
withConnection c U b = b{bUp = c}
withConnection c D b = b{bDown = c}

mkLabel :: Label
mkLabel = MkLabel mempty False False False False

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
    Just (Label _) -> labelToBox m
    _ -> m

addLabelHere :: Model -> Model
addLabelHere m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Nothing ->
      m
        { grid = insert selectedCell (Label mkLabel) grid
        }
    Just (Junction _) -> junctionToLabel m
    Just (Box _) -> boxToLabel m
    _ -> m

boxToLabel :: Model -> Model
boxToLabel m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just (Box (MkBox s u d l r)) ->
      let c = Label $ MkLabel s (fromConn u) (fromConn d) (fromConn l) (fromConn r)
          fromConn None = False
          fromConn _ = True
       in insertCell c m
    _ -> m

labelToBox :: Model -> Model
labelToBox m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just (Label (MkLabel s u d l r)) ->
      let c = Box $ MkBox s (fromConn u) (fromConn d) (fromConn l) (fromConn r)
          fromConn False = None
          fromConn True = Line
       in insertCell c m
    _ -> m

insertCell :: Cell -> Model -> Model
insertCell c m@Model{grid, selectedCell} = m{grid = insert selectedCell c grid}
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
      L -> Box $ b{bLeft = conn}
      R -> Box $ b{bRight = conn}
      U -> Box $ b{bUp = conn}
      D -> Box $ b{bDown = conn}
    Just (Junction j) -> Just $ case dir of
      L -> Junction $ j{jLeft = True}
      R -> Junction $ j{jRight = True}
      U -> Junction $ j{jUp = True}
      D -> Junction $ j{jDown = True}
    Just (Label l) -> Just $ case dir of
      L -> Label $ l{lLeft = True}
      R -> Label $ l{lRight = True}
      U -> Label $ l{lUp = True}
      D -> Label $ l{lDown = True}
    Nothing -> Just $ case dir of
      L -> Junction $ emptyJunction{jLeft = True}
      R -> Junction $ emptyJunction{jRight = True}
      U -> Junction $ emptyJunction{jUp = True}
      D -> Junction $ emptyJunction{jDown = True}

getNeighboringConnection :: Dir -> Model -> Connection
getNeighboringConnection dir Model{grid, selectedCell} =
  case lookup (moveCoord dir selectedCell) grid of
    Just (Box (MkBox{bRight})) | dir == L -> bRight
    Just (Box (MkBox{bLeft})) | dir == R -> bLeft
    Just (Box (MkBox{bDown})) | dir == U -> bDown
    Just (Box (MkBox{bUp})) | dir == D -> bUp
    Just (Junction (MkJunction{jRight = True})) | dir == L -> Line
    Just (Junction (MkJunction{jLeft = True})) | dir == R -> Line
    Just (Junction (MkJunction{jDown = True})) | dir == U -> Line
    Just (Junction (MkJunction{jUp = True})) | dir == D -> Line
    Just (Label (MkLabel{lRight = True})) | dir == L -> Line
    Just (Label (MkLabel{lLeft = True})) | dir == R -> Line
    Just (Label (MkLabel{lDown = True})) | dir == U -> Line
    Just (Label (MkLabel{lUp = True})) | dir == D -> Line
    _ -> None

connectToNeighbors :: Model -> Model
connectToNeighbors m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just (Box b@MkBox{bLeft, bRight, bUp, bDown}) ->
      let connector _ None = None
          connector _ ArrowIn = Line
          connector None Line = ArrowIn
          connector existing Line = existing
       in m
            { grid =
                insert
                  selectedCell
                  ( Box
                      b
                        { bLeft = connector bLeft (getNeighboringConnection L m)
                        , bRight = connector bRight (getNeighboringConnection R m)
                        , bUp = connector bUp (getNeighboringConnection U m)
                        , bDown = connector bDown (getNeighboringConnection D m)
                        }
                  )
                  grid
            }
    Just (Label l) ->
      let
        connector None = False
        connector _ = True
       in
        m
          { grid =
              insert
                selectedCell
                ( Label
                    l
                      { lLeft = connector (getNeighboringConnection L m)
                      , lRight = connector (getNeighboringConnection R m)
                      , lUp = connector (getNeighboringConnection U m)
                      , lDown = connector (getNeighboringConnection D m)
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

junctionToBox :: Model -> Model
junctionToBox m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just Junction{} ->
      let m' = m{grid = insert selectedCell (Box mkBox) grid}
       in connectToNeighbors m'
    _ -> m

junctionToLabel :: Model -> Model
junctionToLabel m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Just Junction{} ->
      let m' = m{grid = insert selectedCell (Label mkLabel) grid}
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
      (Box MkBox{bLeft = None, bRight = None, bUp = None, bDown = None}) ->
        m{grid = delete selectedCell grid}
    Just (Box b) -> m{grid = insert selectedCell (boxToJunction b) grid}
    Just (Label l) -> m{grid = insert selectedCell (labelToJunction l) grid}
    Just (Junction _) -> deleteCell m
    Nothing -> m

boxToJunction :: Box -> Cell
boxToJunction MkBox{bUp, bDown, bLeft, bRight} =
  Junction $
    MkJunction (bUp /= None) (bDown /= None) (bLeft /= None) (bRight /= None)

labelToJunction :: Label -> Cell
labelToJunction MkLabel{lUp, lDown, lLeft, lRight} =
  Junction $
    MkJunction lUp lDown lLeft lRight

deleteCell :: Model -> Model
deleteCell = disconnectNeighbors . removeSelected
 where
  removeSelected m@Model{grid} | size grid == 1 = m
  removeSelected m@Model{grid, selectedCell} = m{grid = delete selectedCell grid}
  disconnectNeighbors m@Model{grid, selectedCell = (x, y)} =
    m{grid = disconnectLeftNeighbor . disconnectRightNeighbor . disconnectUpNeighbor . disconnectDownNeighbor $ grid}
   where
    disconnectLeftNeighbor = disconnect R (x - 1, y)
    disconnectRightNeighbor = disconnect L (x + 1, y)
    disconnectUpNeighbor = disconnect D (x, y - 1)
    disconnectDownNeighbor = disconnect U (x, y + 1)

disconnect :: Dir -> CellCoord -> Grid -> Grid
disconnect L = adjust f
 where
  f (Box b@(MkBox{})) = Box b{bLeft = None}
  f (Junction j@(MkJunction{})) = Junction j{jLeft = False}
  f (Label l@(MkLabel{})) = Label l{lLeft = False}
disconnect R = adjust f
 where
  f (Box b@(MkBox{})) = Box b{bRight = None}
  f (Junction j@(MkJunction{})) = Junction j{jRight = False}
  f (Label l@(MkLabel{})) = Label l{lRight = False}
disconnect U = adjust f
 where
  f (Box b@(MkBox{})) = Box b{bUp = None}
  f (Junction j@(MkJunction{})) = Junction j{jUp = False}
  f (Label l@(MkLabel{})) = Label l{lUp = False}
disconnect D = adjust f
 where
  f (Box b@(MkBox{})) = Box b{bDown = None}
  f (Junction j@(MkJunction{})) = Junction j{jDown = False}
  f (Label l@(MkLabel{})) = Label l{lDown = False}

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
emptyCell = Junction emptyJunction

emptyJunction :: Junction
emptyJunction = MkJunction False False False False

appWidget :: Bool -> RenderModel -> Widget ()
appWidget insertMode m =
  hBox (map (renderColumn insertMode) m)

toWidget :: Bool -> Int -> RenderCell -> Widget ()
toWidget insertMode colWidth (RenderCell{selected, cell = Box b}) = toBoxWidget colWidth selected insertMode b
toWidget _ _ (RenderCell{selected, cell = Junction j}) = toJunctionWidget selected j
toWidget insertMode colWidth (RenderCell{selected, cell = Label l}) = toLabelWidget colWidth selected insertMode l

sampleText :: [Char]
sampleText = "Insert text..."

sampleTextStyle :: Widget n -> Widget n
sampleTextStyle = withAttr sampleTextAttr

editedTextStyle :: Widget n -> Widget n
editedTextStyle = withAttr editedTextAttr

toBoxWidget :: Int -> Bool -> Bool -> Box -> Widget ()
toBoxWidget colWidth selected insertMode b =
  let (contentStyle, content) = case bText b of
        "" -> (sampleTextStyle, sampleText)
        s -> (if selected && insertMode then editedTextStyle else id, s)
      boxWidget =
        withBorderStyle unicode
          . border
          . padAll 1
          . contentStyle
          . str
          $ content
      extraWidth = colWidth - boxWidth content
      upConn = case bUp b of
        None -> str $ replicate colWidth ' '
        Line -> hCenter (str "│")
        ArrowIn -> hCenter (str "▼")
      downConn = case bDown b of
        None -> str $ replicate colWidth ' '
        Line -> hCenter (str "│")
        ArrowIn -> hCenter (str "▲")
      leftConn = str $ case bLeft b of
        None -> spaces
        Line -> hLineL ++ "─"
        ArrowIn -> hLineL ++ "►"
      rightConn = case bRight b of
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

toLabelWidget :: Int -> Bool -> Bool -> Label -> Widget ()
toLabelWidget colWidth selected insertMode l =
  let vLine c = hCenter $ hLimit 1 $ vLimit 3 $ fill c
      topLine =
        if lUp l
          then vLine '│'
          else vLine ' '
      bottomLine =
        if lDown l
          then vLine '│'
          else vLine ' '
      leftLine =
        if lLeft l
          then fill '─'
          else fill ' '
      rightLine =
        str $
          replicate rightLineWidth $
            if lRight l
              then '─'
              else ' '
      contentWidth = textWidth content
      remainingWidth = colWidth - contentWidth
      rightLineWidth = remainingWidth `div` 2
      label = contentStyle . str $ content
      widget = topLine <=> (leftLine <+> label <+> rightLine) <=> bottomLine
      selEdAttr = if insertMode then editedAttr else selectedAttr
      (contentStyle, content) = case lText l of
        "" -> (sampleTextStyle, sampleText)
        s -> (if selected && insertMode then editedTextStyle else id, s)
   in if selected
        then withAttr selEdAttr widget
        else widget

boxWidth :: String -> Int
boxWidth s = textWidth s + 6 -- contents + 2 * padding + 2 * border + 2 * border

columnWidth :: [Cell] -> Int
columnWidth column =
  let texts [] = []
      texts (c : cs) = case c of
        Box b ->
          let content = case bText b of
                "" -> sampleText
                s -> s
           in content : texts cs
        Label l ->
          let content = case lText l of
                "" -> sampleText
                s -> s
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
