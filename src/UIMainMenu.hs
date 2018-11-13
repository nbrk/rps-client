module UIMainMenu where

import           Types
import           UITypes

import           RPS.Types
import           RPS.TypesInstances

import           WEGO

import           Brick
import qualified Brick.AttrMap                as A
import           Brick.BChan
import qualified Brick.Main                   as M
import           Brick.Util
import qualified Brick.Widgets.Center         as C
import qualified Brick.Widgets.Dialog         as D
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Graphics.Vty                 as V


initialMainMenuDialog :: D.Dialog MainMenuChoice
initialMainMenuDialog =
  D.dialog
  (Just "Main menu")
  (Just (1,
         [ ("Connect!", MainMenuChoiceGame)
         , ("Settings", MainMenuChoiceSettings)
         , ("Info", MainMenuChoiceInfo)
         , ("Quit", MainMenuChoiceQuit)
         ]))
  60


renderMainMenu :: ClientState UIState -> [Widget UIName]
renderMainMenu c =
  let d = c ^. clientUI . uiMainMenuDialog
      dw = D.renderDialog d $
           C.hCenter $ padAll 1 $ str "Shall we play a Rock/Paper/Scissors game?"
  in
    [dw]


mainMenuHandleEvent ::
     ClientState UIState
  -> BrickEvent UIName ClientEvent
  -> EventM UIName (Next (ClientState UIState))
mainMenuHandleEvent c event =
  case event of
    VtyEvent ev -> do
      -- process dialog
      d' <- D.handleDialogEvent ev (c ^. clientUI . uiMainMenuDialog)
      let dc = D.dialogSelection d'
      let c' = c & clientUI . uiMainMenuDialog .~ d'

      case ev of
        V.EvKey V.KEsc [] -> M.halt c'
        V.EvKey (V.KChar 'q') [] -> M.halt c'
        V.EvKey V.KEnter [] ->
          case dc of
            Just MainMenuChoiceQuit ->
              M.halt c'
            Just MainMenuChoiceSettings ->
              M.continue $ c' & clientUI . uiCurrentScreen .~ ScreenSettings
            Just MainMenuChoiceInfo ->
              M.continue $ c' & clientUI . uiCurrentScreen .~ ScreenAbout
            Just MainMenuChoiceGame -> do
              -- acquire the net channels
              let h = c' ^. clientSettings . settingsServerName
              let cp = c' ^. clientSettings . settingsControlPort
              let vp = c' ^. clientSettings . settingsViewPort
              (ctrlchan, viewchan) <- liftIO $ spawnClient h (cp, vp)
              void $ liftIO $ forkIO (viewerThread viewchan (c' ^. clientUI . uiChan))
              -- modify client state accordingly
              M.continue $ flip execState c' $ do
                clientConnected .= True
                clientControlChannel .= Just ctrlchan
                clientUI . uiCurrentScreen .= ScreenGame

        _ -> M.continue c'

    AppEvent _ -> M.continue c



viewerThread :: TChan Patch -> BChan ClientEvent -> IO ()
viewerThread viewchan evtchan = forever $ do
  pat <- atomically $ readTChan viewchan
  writeBChan evtchan pat
