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
   in columnWithMeasuredWidth
        [box False "Hi", box False "Mom"]

columnWidth :: [Widget n] -> RenderM n Int
columnWidth ws = do
  results <- mapM render ws
  pure $ maximum (0 : fmap (imageWidth . image) results)

renderColumnWithWidth :: [Widget n] -> RenderM n (Widget n)
renderColumnWithWidth ws = do
  w <- columnWidth ws

  let column = vBox ws
      widthInfo =
        str ("Column width: " <> show w)

  pure $
    vBox
      [ column,
        hBorder,
        widthInfo
      ]

columnWithWidth :: [Widget n] -> Widget n
columnWithWidth ws =
  Widget Fixed Fixed $ do
    w <- columnWidth ws
    render $
      vBox
        [ vBox ws,
          hBorder,
          str ("Column width: " <> show w)
        ]

columnWithMeasuredWidth :: [Widget n] -> Widget n
columnWithMeasuredWidth ws =
  Widget Fixed Fixed $ do
    w <- columnWidth ws
    render $
      hBox
        [ vBox ws,
          padLeft (Pad 1) $
            hLimit w $
              fill '·'
        ]

-- ◄ ► ▲ ▼ ─ │ │ ─ ┬ ┴ ┼ ╭ ╮ ╯ ╰
