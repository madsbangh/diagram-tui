{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.List qualified
import Data.Map hiding (map)
import Data.Maybe
import Data.Text.Lazy (unpack)
import Data.Vector qualified as V
import Graphics.Vty
import Graphics.Vty.PictureToSpans
import Graphics.Vty.Span
import Lens.Micro
import System.Hclip
import Prelude hiding (head, lookup)

data CellSize = Large | Small

data Model = Model
  { grid :: Grid
  , selectedCell :: CellCoord
  , currentMode :: EditorMode
  , clipboard :: Maybe Cell
  , undo :: Maybe Model
  , redo :: Maybe Model
  , showHelp :: Bool
  , cellSize :: CellSize
  }

data Connections = Connections
  { up :: Connection
  , down :: Connection
  , left :: Connection
  , right :: Connection
  }

data EditorMode
  = Normal
  | InsertText
  | PendingDelete
  | CopyToClipboardResult (Either ClipboardException ())

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
main = void $ defaultMain app defaultModel

defaultModel :: Model
defaultModel =
  Model
    { grid = empty
    , selectedCell = (0, 0)
    , currentMode = Normal
    , clipboard = Nothing
    , undo = Nothing
    , redo = Nothing
    , showHelp = True
    , cellSize = Large
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
drawApp m =
  let baseApp = [appWidget (cellSize m) $ toRenderModel m]
      appAndPopup = case currentMode m of
        CopyToClipboardResult r -> resultPopup r : baseApp
        _ -> baseApp
      appAndHelp =
        if showHelp m
          then helpWidget m : appAndPopup
          else appAndPopup
   in appAndHelp

resultPopup :: Either ClipboardException () -> Widget ()
resultPopup r = center $ border $ padAll 1 $ str $ case r of
  (Right ()) -> "Copied to clipboard."
  (Left (UnsupportedOS s)) ->
    "Error: Could not copy to the clipboard. Unsupported OS: " ++ s
  (Left NoTextualData) ->
    "Error: Could not copy to the clipboard. Nothing to copy."
  (Left (MissingCommands cs)) ->
    "Error: Could not copy to the clipboard. Missing commands: "
      ++ Data.List.intercalate ", " cs

modeHelp :: EditorMode -> String
modeHelp m = unlines $ map (commandHelp . snd) (commands m)

helpWidget :: Model -> Widget ()
helpWidget Model{currentMode} =
  let (helpTitle, helpText) = case currentMode of
        Normal -> ("Normal mode", modeHelp Normal)
        InsertText -> ("Insert mode", modeHelp InsertText)
        PendingDelete -> ("Deleting", modeHelp PendingDelete)
        CopyToClipboardResult r -> ("Help", modeHelp $ CopyToClipboardResult r)
   in overrideAttr borderAttr helpAttr
        . withAttr helpAttr
        . floatRightBottom
        . borderWithLabel (str helpTitle)
        . padLeftRight 1
        . str
        $ helpText

floatRightBottom :: Widget n -> Widget n
floatRightBottom p =
  Widget Greedy Greedy $ do
    result <- render p
    c <- getContext
    let rWidth = result ^. imageL . to imageWidth
        rHeight = result ^. imageL . to imageHeight
        leftPaddingAmount = max 0 (c ^. availWidthL - rWidth)
        topPaddingAmount = max 0 (c ^. availHeightL - rHeight)
        paddedImage = translate leftPaddingAmount topPaddingAmount $ result ^. imageL
        off = Location (leftPaddingAmount, topPaddingAmount)
    if leftPaddingAmount == 0 && topPaddingAmount == 0
      then
        return result
      else
        return $
          addResultOffset off $
            result & imageL .~ paddedImage

recordUndo :: Model -> Model
recordUndo m = m{undo = Just m, redo = Nothing}

performUndo :: Model -> Model
performUndo m@Model{undo, showHelp, clipboard} = case undo of
  Just prevModel ->
    prevModel
      { currentMode = Normal
      , redo = Just m
      , showHelp = showHelp
      , clipboard = clipboard
      }
  Nothing -> m

performRedo :: Model -> Model
performRedo m@Model{redo, showHelp, clipboard} =
  (fromMaybe m redo){showHelp = showHelp, clipboard = clipboard}

modifyWithUndo :: (Model -> Model) -> EventM () Model ()
modifyWithUndo f = modify $ f . recordUndo

data Command = Command {commandHelp :: String, commandFunc :: EventM () Model ()}

commandChar :: [Modifier] -> Char -> String -> EventM () Model () -> ((Key, [Modifier]), Command)
commandChar ms c = command ms (KChar c)

command :: [Modifier] -> Key -> String -> EventM () Model () -> ((Key, [Modifier]), Command)
command ms k help f = ((k, ms), Command (showShortcut ms k ++ " ➜ " ++ help) f)

showShortcut :: [Modifier] -> Key -> String
showShortcut ms k = Data.List.intercalate "+" (map showMod ms ++ [showKey k])

showKey :: Key -> String
showKey (KChar c) = [c]
showKey k = Prelude.drop 1 (show k)

showMod :: Modifier -> String
showMod m = '<' : Prelude.drop 1 (show m) ++ ">"

commands :: EditorMode -> [((Key, [Modifier]), Command)]
commands Normal =
  [ commandChar [] '?' "Toggle help" $ modify toggleHelp
  , commandChar [] '+' "Toggle box size" $ modify toggleBoxSize
  , commandChar [] 'h' "Move cursor left" $ modify (moveSelection L)
  , commandChar [] 'l' "Move cursor right" $ modify (moveSelection R)
  , commandChar [] 'k' "Move cursor up" $ modify (moveSelection U)
  , commandChar [] 'j' "Move cursor down" $ modify (moveSelection D)
  , commandChar [] 'g' "Move cursor to top-left" $ modify (moveSelectionTo minCoord)
  , commandChar [] 'G' "Move cursor to bottom-right" $ modify (moveSelectionTo maxCoord)
  , commandChar [] 'H' "Extend connection left" $ modifyWithUndo (addJunction L)
  , commandChar [] 'L' "Extend connection right" $ modifyWithUndo (addJunction R)
  , commandChar [] 'K' "Extend connection up" $ modifyWithUndo (addJunction U)
  , commandChar [] 'J' "Extend connection down" $ modifyWithUndo (addJunction D)
  , commandChar [] 'i' "Make space left" $ modifyWithUndo (makeSpaceAndConnect L)
  , commandChar [] 'a' "Make space right" $ modifyWithUndo (makeSpaceAndConnect R)
  , commandChar [] 'O' "Make space up" $ modifyWithUndo (makeSpaceAndConnect U)
  , commandChar [] 'o' "Make space down" $ modifyWithUndo (makeSpaceAndConnect D)
  , commandChar [] 'b' "Insert box" $ modifyWithUndo (addBoxHere InsertText)
  , commandChar [] 't' "Insert label" $ modifyWithUndo (addLabelHere InsertText)
  , commandChar [] 'c' "Edit text" $ modifyWithUndo changeSelected
  , commandChar [] 'r' "Replace text" $ modifyWithUndo replaceSelected
  , commandChar [] 'd' "Disconnect/Delete..." $ modify (toMode PendingDelete)
  , commandChar [] 'x' "Delete" $ modifyWithUndo deleteSelected
  , commandChar [] 'y' "Yank (Copy)" $ modify yankSelected
  , commandChar [] 'p' "Paste" $ modifyWithUndo paste
  , commandChar [] 'u' "Undo" $ modify performUndo
  , commandChar [MCtrl] 'r' "Redo" $ modify performRedo
  , commandChar [MCtrl] 'c' "Copy to clipboard" copyToClipboard
  , commandChar [] 'q' "Quit" halt
  ]
commands InsertText =
  [ command [] KEsc "Cancel" $ modify cancelInsert
  , command [] KEnter "Confirm" $ modify (toMode Normal)
  ]
commands PendingDelete =
  [ command [] KEsc "Cancel" $ modify (toMode Normal)
  , commandChar [] 'd' "Delete selected" $ modifyWithUndo (toMode Normal . deleteSelected)
  , commandChar [] 'h' "Disconnect left" $ modifyWithUndo (toMode Normal . deleteConnection L)
  , commandChar [] 'l' "Disconnect right" $ modifyWithUndo (toMode Normal . deleteConnection R)
  , commandChar [] 'k' "Disconnect up" $ modifyWithUndo (toMode Normal . deleteConnection U)
  , commandChar [] 'j' "Disconnect down" $ modifyWithUndo (toMode Normal . deleteConnection D)
  ]
commands (CopyToClipboardResult _) =
  [ command [] KEsc "Dismiss popup" $ modify (toMode Normal)
  , command [] KEnter "Dismiss popup" $ modify (toMode Normal)
  ]

makeSpaceAndConnect :: Dir -> Model -> Model
makeSpaceAndConnect _ m | Data.Map.null (grid m) = m
makeSpaceAndConnect dir m =
  fillGap dir . moveSelection dir . moveAllOnSide dir dir $ m

fillGap :: Dir -> Model -> Model
fillGap dir m@Model{grid, selectedCell = (selX, selY)} =
  (go coords m){selectedCell = (selX, selY)}
 where
  coords = case dir of
    _ | dir `elem` [L, R] -> [(selX, y) | y <- [minY grid .. maxY grid]]
    _ | otherwise -> [(x, selY) | x <- [minX grid .. maxX grid]]
  go [] m' = m'
  go (c : cs) m' = go cs (connectToNeighbors m'{selectedCell = c})

moveAllOnSide :: Dir -> Dir -> Model -> Model
moveAllOnSide side towards m@Model{grid, selectedCell = (selX, selY)} =
  m{grid = mapKeys moveIfOnSide grid}
 where
  moveIfOnSide k | isOnSide side k = moveCoord towards k
  moveIfOnSide k = k
  isOnSide L (x, _) = x < selX
  isOnSide R (x, _) = x > selX
  isOnSide U (_, y) = y < selY
  isOnSide D (_, y) = y > selY

toggleHelp :: Model -> Model
toggleHelp m@Model{showHelp} = m{showHelp = not showHelp}

toggleBoxSize :: Model -> Model
toggleBoxSize m@Model{cellSize} =
  m{cellSize = alternate cellSize}
 where
  alternate Large = Small
  alternate Small = Large

copyToClipboard :: EventM () Model ()
copyToClipboard = do
  m <- get
  if Data.Map.null (grid m)
    then return ()
    else do
      let region = diagramRegion m
      let picture = renderWidget Nothing [appWidget (cellSize m) $ toExportableRenderModel m] region
      let ls = renderPictureToLines picture region
      r <- liftIO (try $ setClipboard (unlines ls) :: IO (Either ClipboardException ()))
      modify $ toMode (CopyToClipboardResult r)

renderPictureToLines :: Picture -> DisplayRegion -> [String]
renderPictureToLines pic (w, h) =
  map (trimEnd . renderSpanOps) (V.toList dops)
 where
  dops = displayOpsForPic pic (w, h)
  renderSpanOps sops = concatMap renderSpanOp (V.toList sops)
  renderSpanOp TextSpan{textSpanText} = unpack textSpanText
  renderSpanOp (Skip n) = replicate n ' '
  renderSpanOp (RowEnd _) = mempty

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

diagramRegion :: Model -> DisplayRegion
diagramRegion m@Model{cellSize} =
  let RenderModel{renderColumns} = toExportableRenderModel m
      w = sum (map (columnWidth . map cell) renderColumns)
      h = maximum (map (columnHeight cellSize . map cell) renderColumns)
   in (w, h)

updateApp :: BrickEvent () e -> EventM () Model ()
updateApp (VtyEvent (EvKey key mods)) = do
  mode <- currentMode <$> get
  case Data.List.lookup (key, mods) (commands mode) of
    Just (Command{commandFunc}) -> commandFunc
    Nothing -> case mode of
      InsertText -> do
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
      _ -> return ()
updateApp _ = return ()

cancelInsert :: Model -> Model
cancelInsert m@Model{undo = beforeInsert} = case beforeInsert of
  Just prevModel -> prevModel{currentMode = Normal}
  Nothing -> m

paste :: Model -> Model
paste m@Model{clipboard} =
  case clipboard of
    Just (Junction cs) -> connectNeighborsToSelection . insertCell (Junction cs) $ m
    Just (Box s _) -> setText s . addBoxHere Normal $ m
    Just (Label s _) -> setText s . addLabelHere Normal $ m
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
moveSelection dir m@Model{selectedCell, grid}
  | Data.Map.null grid =
      let (newX, newY) = moveCoord dir selectedCell
          newX' = max 0 newX
          newY' = max 0 newY
       in m{selectedCell = (newX', newY')}
  | otherwise = m{selectedCell = moveCoord dir selectedCell}

moveSelectionTo :: (((Int, Int) -> Int) -> Grid -> Int) -> Model -> Model
moveSelectionTo _ m@Model{grid} | Data.Map.null grid = m
moveSelectionTo f m@Model{grid} = m{selectedCell = (f fst grid, f snd grid)}

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

addBoxHere :: EditorMode -> Model -> Model
addBoxHere modeAfterCreating m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Nothing ->
      m
        { grid = insert selectedCell mkBox grid
        , currentMode = modeAfterCreating
        }
    Just (Junction _) -> toMode modeAfterCreating $ junctionToBox m
    Just (Label _ _) -> labelToBox m
    _ -> m

addLabelHere :: EditorMode -> Model -> Model
addLabelHere modeAfterCreating m@Model{grid, selectedCell} =
  case lookup selectedCell grid of
    Nothing ->
      m
        { grid = insert selectedCell mkLabel grid
        , currentMode = modeAfterCreating
        }
    Just (Junction _) -> toMode modeAfterCreating $ junctionToLabel m
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
    Nothing -> tryCloseGaps m

tryCloseGaps :: Model -> Model
tryCloseGaps orig@Model{grid, selectedCell = (selX, selY)} =
  tryCloseGap fst selX R . tryCloseGap snd selY D $ orig
 where
  tryCloseGap selector selCoord side m =
    if all ((/= selCoord) . selector) $ keys grid
      then closeGap side m
      else m
  closeGap side = moveAllOnSide side (opposite side)

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

toExportableRenderModel :: Model -> RenderModel
toExportableRenderModel Model{grid} =
  let renderCell c = RenderCell c False
      getCellOrEmpty (x, y) = fromMaybe emptyCell (lookup (x, y) grid)
   in RenderModel
        False
        [ [ renderCell (getCellOrEmpty (x, y))
          | y <- [minY grid .. maxY grid]
          ]
        | x <- [minX grid .. maxX grid]
        ]

toRenderModel :: Model -> RenderModel
toRenderModel Model{grid, selectedCell = (selX, selY), currentMode} =
  let renderCell ((x, y), c) = RenderCell c (x == selX && y == selY)
      getCellOrEmpty (x, y) = fromMaybe emptyCell (lookup (x, y) grid)
      cellAtSelection = findWithDefault emptyCell (selX, selY) grid
      gridWithSelection = insert (selX, selY) cellAtSelection grid
      gridWithSelectionAndOrigin = insert (0, 0) emptyCell gridWithSelection
   in RenderModel
        (case currentMode of InsertText -> True; _ -> False)
        [ [ renderCell ((x, y), getCellOrEmpty (x, y))
          | y <- [minY gridWithSelectionAndOrigin .. maxY gridWithSelectionAndOrigin]
          ]
        | x <- [minX gridWithSelectionAndOrigin .. maxX gridWithSelectionAndOrigin]
        ]

emptyCell :: Cell
emptyCell = Junction disconnected

appWidget :: CellSize -> RenderModel -> Widget ()
appWidget s RenderModel{insertMode, renderColumns} =
  viewport () Both $
    hBox (map (renderColumn s insertMode) renderColumns)

toWidget :: CellSize -> Bool -> Int -> RenderCell -> Widget ()
toWidget siz insertMode colWidth (RenderCell{selected, cell = Box s cs}) = toBoxWidget siz colWidth selected insertMode s cs
toWidget siz insertMode colWidth (RenderCell{selected, cell = Label s cs}) = toLabelWidget siz colWidth selected insertMode s cs
toWidget siz _ _ (RenderCell{selected, cell = Junction cs}) = toJunctionWidget siz selected cs

sampleText :: [Char]
sampleText = "Insert text..."

sampleTextStyle :: Widget n -> Widget n
sampleTextStyle = withAttr sampleTextAttr

editedTextStyle :: Widget n -> Widget n
editedTextStyle = withAttr editedTextAttr

toBoxWidget :: CellSize -> Int -> Bool -> Bool -> String -> Connections -> Widget ()
toBoxWidget cellSize colWidth selected insertMode s cs =
  let (contentStyle, content) = case s of
        "" -> (sampleTextStyle, sampleText)
        _ -> (if selected && insertMode then editedTextStyle else id, s)
      padding = case cellSize of
        Large -> padAll 1
        Small -> padLeftRight 1
      boxWidget =
        withBorderStyle unicode
          . border
          . padding
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
            visible
              . overrideAttr borderAttr selEdAttr
              . withAttr selEdAttr
          else id
   in withSelection $
        upConn
          <=> ( vCenter leftConn
                  <+> boxWidget
                  <+> vCenter rightConn
              )
          <=> downConn

toJunctionWidget :: CellSize -> Bool -> Connections -> Widget ()
toJunctionWidget s selected j =
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
      lineHeight = case s of
        Large -> 3
        Small -> 2
      vLine c = hCenter $ hLimit 1 $ vLimit lineHeight $ fill c
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
        then visible $ withAttr selectedAttr widget
        else widget

toLabelWidget :: CellSize -> Int -> Bool -> Bool -> String -> Connections -> Widget ()
toLabelWidget siz colWidth selected insertMode s cs =
  let lineHeight = case siz of
        Large -> 3
        Small -> 2
      vLine c = hCenter $ hLimit 1 $ vLimit lineHeight $ fill c
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
        then visible $ withAttr selEdAttr widget
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

columnHeight :: CellSize -> [Cell] -> Int
columnHeight s = (* cellHeight s) . length

cellHeight :: CellSize -> Int
cellHeight Large = 7
cellHeight Small = 5

renderColumn :: CellSize -> Bool -> RenderColumn -> Widget ()
renderColumn s insertMode column =
  Widget Fixed Fixed $ do
    let cw = columnWidth (map cell column)
    render $
      hLimit cw $
        vBox (map (vLimit (cellHeight s) . hCenter . toWidget s insertMode cw) column)
