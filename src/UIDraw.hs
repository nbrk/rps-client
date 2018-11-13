module UIDraw where

import           Types
import           UIAbout
import           UIGame
import           UIMainMenu
import           UISettings
import           UITypes

import           Brick
import qualified Brick.AttrMap        as A
import           Brick.Util
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty         as V
import           Lens.Micro


uiDraw :: ClientState UIState -> [Widget UIName]
uiDraw c =
  case c ^. clientUI . uiCurrentScreen of
    ScreenMainMenu ->
      renderMainMenu c
    ScreenGame ->
      renderGame c
    ScreenSettings ->
      renderSettings c
    ScreenAbout ->
      renderAbout c
