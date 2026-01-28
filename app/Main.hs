{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Table
import Control.Monad
import Graphics.Vty

type Model = Bool

main :: IO ()
main = void $ defaultMain app False

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
drawApp isRight = [appWidget isRight]

updateApp :: BrickEvent () e -> EventM () Model ()
updateApp (VtyEvent (EvKey (KChar 'h') [])) = modify (const False)
updateApp (VtyEvent (EvKey (KChar 'l') [])) = modify (const True)
updateApp (VtyEvent (EvKey (KChar 'q') [])) = halt
updateApp (VtyEvent (EvKey KEsc [])) = halt
updateApp _ = return ()

appWidget :: Bool -> Widget ()
appWidget isRight =
  let box borderStyle = withBorderStyle borderStyle . border . padAll 1 . str
      leftStyle = if isRight then unicode else unicodeBold
      rightStyle = if isRight then unicodeBold else unicode
      hello = box leftStyle "Hello"
      arrow1 = connectingWidget Arrow Line None None
      arrow2 = connectingWidget None None Arrow Line
      world = box rightStyle "World afe sef sef se fsef sef"
   in center $
        renderTable $
          centeredBorderlessTable
            [ [hello, arrow1, world],
              [arrow2, emptyWidget, emptyWidget],
              [hello, emptyWidget, emptyWidget],
              [connectingWidget None None Line Line, connectingWidget None None Arrow Arrow, connectingWidget None Line None Line],
              [connectingWidget Line Line None None, connectingWidget Arrow Arrow None None, connectingWidget Line None Line None]
            ]

centeredBorderlessTable :: [[Widget ()]] -> Table ()
centeredBorderlessTable =
  surroundingBorder False
    . rowBorders False
    . columnBorders False
    . setDefaultColAlignment AlignCenter
    . setDefaultRowAlignment AlignMiddle
    . table

data LineEnd = None | Line | Arrow

connectingWidget :: LineEnd -> LineEnd -> LineEnd -> LineEnd -> Widget ()
connectingWidget l r u d =
  let spaces = replicate 6 ' '
      leftEnd = case l of
        None -> " "
        Line -> "─"
        Arrow -> "◄"
      rightEnd = case r of
        None -> " "
        Line -> "─"
        Arrow -> "►"
      topEnd = case u of
        None -> " "
        Line -> "│"
        Arrow -> "▲"
      bottomEnd = case d of
        None -> " "
        Line -> "│"
        Arrow -> "▼"
      topLine = case u of
        None -> " "
        _ -> "│"
      bottomLine = case d of
        None -> " "
        _ -> "│"
      leftLine = case l of
        None -> replicate 5 ' '
        _ -> replicate 5 '─'
      rightLine = case r of
        None -> replicate 5 ' '
        _ -> replicate 5 '─'
      isSomething e = case e of
        None -> False
        _ -> True
      middle = case (isSomething l, isSomething r, isSomething u, isSomething d) of
        (False, False, True, True) -> "│"
        (True, True, False, False) -> "─"
        (False, True, True, True) -> "├"
        (True, False, True, True) -> "┤"
        (True, True, False, True) -> "┬"
        (True, True, True, False) -> "┴"
        (True, True, True, True) -> "┼"
        (False, True, False, True) -> "╭"
        (True, False, False, True) -> "╮"
        (True, False, True, False) -> "╯"
        (False, True, True, False) -> "╰"
        _ -> " "
   in str . unlines $
        [ spaces ++ topEnd ++ spaces,
          spaces ++ topLine ++ spaces,
          spaces ++ topLine ++ spaces,
          leftEnd ++ leftLine ++ middle ++ rightLine ++ rightEnd,
          spaces ++ bottomLine ++ spaces,
          spaces ++ bottomLine ++ spaces,
          spaces ++ bottomEnd ++ spaces
        ]
