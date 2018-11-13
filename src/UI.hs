{-# LANGUAGE OverloadedStrings #-}
module UI where

import           Types
import           UIAttrMap
import           UIDraw
import           UIGame
import           UIHandleEvent
import           UIMainMenu
import           UISettings
import           UITypes

import           RPS.Types
import           RPS.TypesInstances

import           WEGO

import           Brick.BChan
import           Brick.Focus
import           Brick.Forms
import           Brick.Main
import           Graphics.Vty


initialSettings = ClientSettings
        { _settingsServerName = "localhost"
        , _settingsControlPort = 20000
        , _settingsViewPort = 30000
        , _settingsPlayerSide = Player1
        }

initialState eventChan = ClientState
        { _clientGameState = initial
        , _clientDecision = Rock
        , _clientSettings = initialSettings
        , _clientConnected = False
        , _clientControlChannel = Nothing
        , _clientUI = initialUIState eventChan
        }

initialUIState :: BChan ClientEvent -> UIState
initialUIState chan = UIState
  { _uiCurrentScreen = ScreenMainMenu
  , _uiChan = chan

  , _uiMainMenuDialog = initialMainMenuDialog
  , _uiSettingsForm = initialSettingsForm initialSettings
  , _uiGameForm = initialGameForm Rock
  }



theApp :: App (ClientState UIState) ClientEvent UIName
theApp = App
  { appDraw = uiDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent = uiHandleEvent
  , appStartEvent = return
  , appAttrMap = uiAttrMap
  }


startUI :: IO ()
startUI = do
  eventChan <- newBChan 10

  let buildVty = do
        v <- mkVty =<< standardIOConfig
        setMode (outputIface v) Mouse True
        return v

  finalState <- customMain
--    (mkVty defaultConfig)
    buildVty
    (Just eventChan) theApp (initialState eventChan)

  return ()
--  void $ M.defaultMain theApp initialClientState

