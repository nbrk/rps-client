{-# LANGUAGE OverloadedStrings #-}
module OldUI where

import           Types

import           RPS.Types

import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Lens.Micro

import qualified Graphics.Vty         as V

import qualified Brick.Main           as M
import           Brick.Types
import           Brick.Widgets.Core

import qualified Brick.AttrMap        as A
import qualified Brick.Focus          as FC
import qualified Brick.Forms          as F
import qualified Brick.Types          as T
import           Brick.Util
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L


--main = startUI

startUI :: IO ()
startUI = do
  void $ M.defaultMain theApp initialClientState


theApp :: M.App ClientState () ResourceName
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
--          , M.appChooseCursor = FC.focusRingCursor (\c -> c ^. cliSettingsForm . to F.formFocus)
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

drawUI :: ClientState -> [Widget ResourceName]
drawUI c | c ^. cliScreenMode == WelcomeScreen =
           [C.vCenter $ C.hCenter welcomeScreen]
drawUI c | c ^. cliScreenMode == MenuScreen =
           [C.vCenter $ C.hCenter mw]
  where
    renderElement selected e = XXX
    mw = L.renderList renderElement
drawUI c | c ^. cliScreenMode == SettingsScreen =
           [C.vCenter $ C.hCenter fw]
  where
    fw = B.borderWithLabel (str "Settings") $ padTop (Pad 1) $ hLimit 50
         $ F.renderForm $ c ^. cliSettingsForm


appEvent ::
     ClientState
  -> BrickEvent ResourceName ()
  -> T.EventM ResourceName (T.Next ClientState)
appEvent c ev | c ^. cliScreenMode == WelcomeScreen =
  case ev of
    VtyEvent (V.EvKey _ _)  ->
      M.continue $ c & cliScreenMode .~ SettingsScreen
    _                  -> M.continue c
appEvent c ev | c ^. cliScreenMode == SettingsScreen =
  case ev of
    VtyEvent (V.EvKey V.KEsc [])  -> M.halt c
    _                  -> do
      fw' <- F.handleFormEvent ev (c ^. cliSettingsForm)
      M.continue $ c & cliSettingsForm .~ fw'


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (F.invalidFormInputAttr, V.white `on` V.red)
  , (F.focusedFormInputAttr, V.black `on` V.yellow)
  , ("Player1", V.white `on` V.red)
  , ("Player2", V.white `on` V.blue)
  ]

