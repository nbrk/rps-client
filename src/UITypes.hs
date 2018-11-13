{-# LANGUAGE TemplateHaskell #-}
module UITypes where

import           Types

import           RPS.Types

-- import qualified Data.Text            as T
-- import           Data.Vector
import           Lens.Micro
import           Lens.Micro.TH

import qualified Brick.Forms          as F
--import           Brick.Types
--import           Brick.Util
--import qualified Brick.Widgets.Border as B
--import qualified Brick.Widgets.Center as C
--import           Brick.Widgets.Core
import           Brick.BChan
import qualified Brick.Widgets.Dialog as D
--import qualified Brick.Widgets.Edit   as E
--import qualified Brick.Widgets.List   as L


data UIScreen =
    ScreenMainMenu
  | ScreenGame
  | ScreenSettings
  | ScreenAbout
  deriving (Eq)

data UIName =
    ServerNameField
  | ControlPortField
  | ViewPortField
  | Player1SideField
  | Player2SideField
  | RockField
  | PaperField
  | ScissorsField
  deriving (Ord, Eq, Show)

data MainMenuChoice =
    MainMenuChoiceGame
  | MainMenuChoiceSettings
  | MainMenuChoiceInfo
  | MainMenuChoiceQuit
  deriving (Eq)


data UIState = UIState
  { _uiCurrentScreen  :: UIScreen
  , _uiChan           :: BChan ClientEvent

  , _uiMainMenuDialog :: D.Dialog MainMenuChoice
  , _uiSettingsForm   :: F.Form ClientSettings ClientEvent UIName
  , _uiGameForm       :: F.Form Decision ClientEvent UIName
  }
makeLenses ''UIState
