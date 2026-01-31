{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad
import Graphics.Vty

type Model = ()

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

cellHeight :: Int
cellHeight = 5

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
  let box True = withBorderStyle unicodeBold . border . padAll 1 . str
      box False = withBorderStyle unicode . border . padAll 1 . str
   in boxesAndLinesColumn
        [box False "Hi!", box False "World"]

withLinesLeftAndRight :: Widget n -> Widget n
withLinesLeftAndRight widget = hLine <+> widget <+> hLine

hLine :: Widget n
hLine = vCenter (vLimit 1 $ fill '─')

withLinesAboveBelow :: [Widget n] -> [Widget n]
withLinesAboveBelow (c : olumn) = vLine : c : vLine : withLinesAboveBelow olumn
withLinesAboveBelow [] = []

vLine :: Widget n
vLine = hCenter (str "│")

columnWidth :: [Widget n] -> RenderM n Int
columnWidth ws = do
  results <- mapM render ws
  pure $ maximum (0 : fmap (imageWidth . image) results)

boxesAndLinesColumn :: [Widget n] -> Widget n
boxesAndLinesColumn ws =
  Widget Fixed Fixed $ do
    width <- columnWidth (filter isFixeWidth ws)
    render $
      hLimit (width + 2) $
        vBox (map (vLimit cellHeight . hCenter) (withLinesAboveBelow ws))

isFixeWidth :: Widget n -> Bool
isFixeWidth (Widget w _ _) = w == Fixed

-- ◄ ► ▲ ▼ ─ │ │ ─ ┬ ┴ ┼ ╭ ╮ ╯ ╰
