module UIHandleEvent where

import           Types
import           UIAbout
import           UIGame
import           UIMainMenu
import           UISettings
import           UITypes

import qualified Brick.AttrMap          as A
import           Brick.BChan
import qualified Brick.Main             as M
import           Brick.Types
import           Brick.Util
import qualified Brick.Widgets.Dialog   as D
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Graphics.Vty           as V
import           Lens.Micro


uiHandleEvent ::
     ClientState UIState
  -> BrickEvent UIName ClientEvent
  -> EventM UIName (Next (ClientState UIState))
uiHandleEvent c event =
  case c ^. clientUI . uiCurrentScreen of
    ScreenMainMenu -> mainMenuHandleEvent c event
    ScreenGame     -> gameHandleEvent c event
    ScreenSettings -> settingsHandleEvent c event
    ScreenAbout    -> aboutHandleEvent c event
