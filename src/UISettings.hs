{-# LANGUAGE OverloadedStrings #-}
module UISettings where

import           Types
import           UITypes

import           Brick
import qualified Brick.AttrMap        as A
import           Brick.Forms
import qualified Brick.Main           as M
import           Brick.Util
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.Text            as T
import qualified Graphics.Vty         as V


initialSettingsForm :: ClientSettings -> Form ClientSettings ClientEvent UIName
initialSettingsForm =
  let label s w =
        (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
  in
  newForm
  [ label "Server name: " @@= editShowableField settingsServerName ServerNameField
  , label "Control port: " @@= editShowableField settingsControlPort ControlPortField
  , label "View port: " @@= editShowableField settingsViewPort ViewPortField
  , label "Player side: " @@= radioField settingsPlayerSide
    [ (Player1, Player1SideField, "Player 1")
    , (Player2, Player2SideField, "Player 2")
    ]
  ]


renderSettings :: ClientState UIState -> [Widget UIName]
renderSettings c =
  let w = renderForm (c ^. clientUI . uiSettingsForm)
  in
    [C.center $ hLimit 46 $
     B.borderWithLabel
     (str "Settings")
     (padAll 1 $ str "Focus: TAB, Select: SPC, Accept form: RET" <=> str " " <=> w)]



settingsHandleEvent ::
     ClientState UIState
  -> BrickEvent UIName ClientEvent
  -> EventM UIName (Next (ClientState UIState))
settingsHandleEvent c event = do
  f' <- handleFormEvent event (c ^. clientUI . uiSettingsForm)

  M.continue $ flip execState c $ do
    clientUI . uiSettingsForm .= f'
    when (event == VtyEvent (V.EvKey V.KEsc [])) $
      clientUI . uiCurrentScreen .= ScreenMainMenu
    when (event == VtyEvent (V.EvKey V.KEnter []) && allFieldsValid f') $ do
      clientSettings .= formState f'
      clientUI . uiCurrentScreen .= ScreenMainMenu


--    AppEvent _ -> M.continue c
