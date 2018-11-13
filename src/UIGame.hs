{-# LANGUAGE OverloadedStrings #-}
module UIGame where

import           Types
import           UITypes

import           RPS.Types
import           RPS.TypesInstances

import           WEGO

import           Brick
import qualified Brick.AttrMap                as A
import           Brick.Forms
import qualified Brick.Main                   as M
import           Brick.Util
import qualified Brick.Widgets.Border         as B
import qualified Brick.Widgets.Center         as C
import           Brick.Widgets.Core
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text                    as T
import qualified Graphics.Vty                 as V


initialGameForm :: Decision -> Form Decision ClientEvent UIName
initialGameForm =
  let label s w =
        (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
  in
  newForm
  [ label "Your decision: " @@= radioField id
    [ (Rock, RockField, "ROCK")
    , (Paper, PaperField, "PAPER")
    , (Scissors, ScissorsField, "SCISSORS")
    ]
  ]


opponentOf :: PlayerSide -> PlayerSide
opponentOf Player1 = Player2
opponentOf Player2 = Player1


playerReady :: Game -> PlayerSide -> Bool
playerReady g Player1 = isJust $ _gamePlayer1 g
playerReady g Player2 = isJust $ _gamePlayer2 g

descrOutcome :: Outcome -> String
descrOutcome Draw       = "Nobody"
descrOutcome WinPlayer1 = "Player 1"
descrOutcome WinPlayer2 = "Player 2"


renderGame :: ClientState UIState -> [Widget UIName]
renderGame c | not (c ^. clientConnected) =
               [C.center $ hLimit 30 $ B.border (str "Connecting...")]
renderGame c | c ^. clientConnected =
  let wf = renderForm (c ^. clientUI . uiGameForm)
      wc = str $ "Connected to "
        ++ c ^. clientSettings . settingsServerName
        ++ ", game round " ++ show (_gameRound g)
      wd = (str "You are playing as: ") <+>
           str (show side)
      wb = str " "
      ws = str $ "Your decision on server: " ++ show dec
      side = c ^. clientSettings . settingsPlayerSide
      opp = opponentOf side
      g = c ^. clientGameState
      o = _gameOutcome g
      dec = getDecision side g
      wo = str "Opponent status: " <+>
             if playerReady g opp
             then str "READY"
             else str "THINKING"
      wl = str "Last winner: " <+> str (descrOutcome o)
      p1wins = g ^. gamePlayer1Wins
      p2wins = g ^. gamePlayer2Wins
      wt = B.borderWithLabel (str "Total wins") $
             str ("Player 1: " ++ show p1wins) <+>
             str "  |  " <+>
             str ("Player 2: " ++ show p2wins)
  in
    [ C.center $ hLimit 60 $
      B.borderWithLabel
      (str "Game")
      (padAll 1 $ wc <=> wb <=> wd <=>  ws <=> wf
        <=> wb <=> wo <=> wb <=> wl <=> wb <=> wt) ]


gameHandleEvent ::
     ClientState UIState
  -> BrickEvent UIName ClientEvent
  -> EventM UIName (Next (ClientState UIState))
gameHandleEvent c event = do
  f' <- handleFormEvent event (c ^. clientUI . uiGameForm)
  let c' = c & clientUI . uiGameForm .~ f'

  case event of
    VtyEvent ev -> do
      case ev of
        V.EvKey V.KEsc [] ->
          M.continue $ c' & clientUI . uiCurrentScreen .~ ScreenMainMenu
        V.EvKey (V.KChar 'q') [] ->
          M.continue $ c' & clientUI . uiCurrentScreen .~ ScreenMainMenu
        V.EvKey V.KEnter [] -> do
          let dec = formState f'
          let ctrlchan = fromJust $ c' ^. clientControlChannel
          let side = c' ^. clientSettings . settingsPlayerSide
          let g = c' ^. clientGameState
          let g' = makeDecision side  (Just dec) g
          let pat = diff g g'
          liftIO $ atomically $ writeTChan ctrlchan pat
          M.continue $ c' & clientGameState .~ g'

        _ -> M.continue c'

    AppEvent pat -> do
      let g = c' ^. clientGameState
      case patch pat g of
        Just g' ->
          M.continue $ c' & clientGameState .~ g'
        Nothing ->
          -- error: incoming patch is incorrect!
          M.continue $ c' & clientGameState .~ g


makeDecision :: PlayerSide-> Maybe Decision -> Game -> Game
makeDecision Player1 md g = g { _gamePlayer1 = md }
makeDecision Player2 md g = g { _gamePlayer2 = md }


getDecision :: PlayerSide -> Game -> Maybe Decision
getDecision Player1 = _gamePlayer1
getDecision Player2 = _gamePlayer2

  -- M.continue $ flip execState c $ do
  --   -- state monad
  --   clientUI . uiGameForm .= f'
  --   case event of
  --     VtyEvent (V.EvKey V.KEsc []) ->
  --       clientUI . uiCurrentScreen .= ScreenMainMenu
  --     VtyEvent (V.EvKey (V.KChar 'q') []) ->
  --       clientUI . uiCurrentScreen .= ScreenMainMenu

  --     AppEvent (ClientEventPatch pat) -> do
  --       clientConnected .= True
  --       g <- use clientGameState
  --       let mbg = patch pat g
  --       case mbg of
  --         Just g' -> clientGameState .= g'
  --         Nothing ->
  --           -- error: incoming patch is incorrect!
  --           clientGameState .= g
