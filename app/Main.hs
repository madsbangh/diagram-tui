module Main (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
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
      arrow = padTopBottom 2 . str $ "-->"
      world = box rightStyle "World"
   in center (hello <+> arrow <+> world)
