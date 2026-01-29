{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Table
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

centeredBorderlessTable :: [[Widget ()]] -> Widget ()
centeredBorderlessTable =
  center
    . renderTable
    . surroundingBorder False
    . rowBorders False
    . columnBorders False
    . setDefaultColAlignment AlignCenter
    . setDefaultRowAlignment AlignMiddle
    . table

appWidget :: Model -> Widget ()
appWidget _ =
  let box True = withBorderStyle unicodeBold . border . padAll 1 . str
      box False = withBorderStyle unicode . border . padAll 1 . str
   in centeredColumnWithFixedWidth
        [box False "Hi", box False "World!"]

columnWidth :: [Widget n] -> RenderM n Int
columnWidth ws = do
  results <- mapM render ws
  pure $ maximum (0 : fmap (imageWidth . image) results)

centeredColumnWithFixedWidth :: [Widget n] -> Widget n
centeredColumnWithFixedWidth ws =
  Widget Fixed Fixed $ do
    width <- columnWidth (filter isFixeWidth ws)
    render $
      hLimit width $
        vLimit cellHeight $
          vBox (map hCenter ws)

isFixeWidth :: Widget n -> Bool
isFixeWidth (Widget w _ _) = w == Fixed

-- ◄ ► ▲ ▼ ─ │ │ ─ ┬ ┴ ┼ ╭ ╮ ╯ ╰
