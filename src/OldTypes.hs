{-# LANGUAGE TemplateHaskell #-}
module OldTypes where

import           RPS.Types

import qualified Data.Text            as T
import           Data.Vector
import           Lens.Micro
import           Lens.Micro.TH

import qualified Brick.Forms          as F
import           Brick.Types
import           Brick.Util
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L



-- | Player side in the game
data Side = Player1 | Player2
  deriving (Show, Eq)


-- | Settings form
data Settings = Settings
  { _settingsHost  :: T.Text
  , _settingsCPort :: Int
  , _settingsVPort :: Int
  , _settingsSide  :: Side
  } deriving (Show)
makeLenses ''Settings

data ResourceName = HostField
          | CPortField
          | VPortField
          | Player1Field
          | Player2Field
          | MainMenu
          deriving (Eq, Ord, Show)


data ScreenMode =
    WelcomeScreen
  | MenuScreen
  | SettingsScreen
  | GameScreen
  deriving (Eq)


data MenuSelection = MenuGame | MenuSettings | MenuQuit
  deriving (Eq)


data ClientState = ClientState
  { _cliGame         :: Maybe Game
  , _cliSettings     :: Settings
  , _cliScreenMode   :: ScreenMode

  --
  -- Static widgets
  --
  , _cliMainMenu     :: L.List ResourceName MenuSelection
  , _cliSettingsForm :: F.Form Settings () ResourceName
  }

makeLenses ''ClientState


initialSettings = Settings (T.pack "127.0.0.1") 20000 30000 Player1


initialClientState = ClientState
  { _cliGame = Nothing

  , _cliScreenMode = WelcomeScreen
  , _cliSettings = initialSettings

  , _cliMainMenu = mainMenu
  , _cliSettingsForm = mkSettingsForm initialSettings
  }


-----

mkSettingsForm :: Settings -> F.Form Settings e ResourceName
mkSettingsForm =
  let str' s w = padBottom (Pad 1) $
                  (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
  in
    F.newForm
    [ str' "Server address: "
      F.@@= F.editTextField settingsHost HostField (Just 1)

    , str' "Control port: "
      F.@@= F.editShowableField settingsCPort CPortField

    , str' "View Port: "
      F.@@= F.editShowableField settingsVPort VPortField

    , str' "Play as side: "
      F.@@= F.radioField settingsSide
      [ (Player1, Player1Field, T.pack "Player 1")
      , (Player2, Player2Field, T.pack "Player 2")
      ]
    ]


welcomeScreen :: Widget ResourceName
welcomeScreen =
  B.borderWithLabel (str "Welcome") $
  padTop (Pad 1) $
  hLimit 50 $
  (str "WEGO Rock-Paper-Scissors client.") <=> (str " ") <=>
  (strWrap
   "The program connects to the Rock-Paper-Scissors server and issues/monitors/draws the deltas.") <=>
  (str " ") <=>
  (strWrap
   "The game is played in WEGO mode: each player patches the game state whenever he wants, but the turn simulation (the endomorphism) is done only after all of the players check themselfes as 'ready' in the state.") <=>
  (str " ") <=>
  (strWrap
   "You must choose the side to play.")


mainMenu :: L.List ResourceName MenuSelection
mainMenu = L.list
           MainMenu
           (fromList [MenuGame, MenuSettings, MenuQuit])
           3
