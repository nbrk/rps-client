module UIAbout where

import           Types
import           UITypes

import           Brick
import qualified Brick.AttrMap        as A
import qualified Brick.Main           as M
import           Brick.Util
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Graphics.Vty         as V


aboutWidget :: Widget UIName
aboutWidget =
  C.center $ hLimit 46 $
  B.borderWithLabel
  (str "About") $
  padAll 1 $
  (strWrap "This is the Rock/Paper/Scissors game client. It demonstrates WEGO Client/Server architecture.") <=>
  (str " ") <=>
  (strWrap ("The Game State is distributed to all peers via incremental patches. " ++
  "The clients diff the State against their own modifications and likewise upload patches to the server.")) <=>
  (str " ") <=>
  (strWrap "Everything is completely asynchronous.")



renderAbout :: ClientState UIState -> [Widget UIName]
renderAbout c =
  let w = aboutWidget
  in
    [w]



aboutHandleEvent ::
     ClientState UIState
  -> BrickEvent UIName ClientEvent
  -> EventM UIName (Next (ClientState UIState))
aboutHandleEvent c event =
  case event of
    VtyEvent ev ->
      M.continue $
        flip execState c $
        case ev of
          V.EvKey _ _ ->
            clientUI . uiCurrentScreen .= ScreenMainMenu

    AppEvent _ -> M.continue c
