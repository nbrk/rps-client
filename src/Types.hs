{-# LANGUAGE TemplateHaskell #-}
module Types where

import           RPS.Types

import           Control.Concurrent.STM.TChan
import           Lens.Micro
import           Lens.Micro.TH


data PlayerSide = Player1 | Player2
  deriving (Eq)

instance Show PlayerSide where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

data ClientSettings = ClientSettings
  { _settingsServerName  :: String
  , _settingsControlPort :: Int
  , _settingsViewPort    :: Int
  , _settingsPlayerSide  :: PlayerSide
  }
makeLenses ''ClientSettings


data ClientState ui = ClientState
  { _clientGameState      :: Game
  , _clientDecision       :: Decision
  , _clientSettings       :: ClientSettings
  , _clientConnected      :: Bool
  , _clientControlChannel :: Maybe (TChan Patch)
  , _clientUI             :: ui
  }
makeLenses ''ClientState


-- General client events
type ClientEvent = Patch

