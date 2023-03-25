module CommandLine.CommandLine 
    ( requestParser
    ) where

import Options.Applicative
import Types (AuthToken, mkAuthToken, Request (..), Command (..))
import Data.Text as T
import Text.Read (readMaybe)

requestParser:: Parser Request
requestParser = Request
      <$> commandParser
      <*> option parseInitialAuthToken
          ( long "authToken"
         <> short 'a'
         <> help "An existing authentication token to use when communicating with the nanoleafs."
         <> value Nothing
         <> metavar "AUTH")

commandParser :: Parser Command
commandParser = subparser
        ( command "show-all-panel-info" 
            (info (pure GetAllPanelInfo) (progDesc "Display all information about the panels."))
        <> command "new-auth-token" 
            (info (pure GetNewAuthToken) (progDesc "Request a new authentication token. (Hold the power button until panels start blinking)"))
        <> command "turn-off" 
            (info (pure TurnOff) (progDesc "Turn off panels if they are on."))
        <> command "turn-on" 
            (info (pure TurnOn) (progDesc "Turn on panels if they are off."))
        <> command "show-brightness" 
            (info (pure ShowBrightness) (progDesc "See current brightness of the panels."))
        <> command "set-brightness" 
            (info 
                (argument (maybeReader (fmap SetBrightness . readMaybe)) 
                    (metavar "INT"
                    <> help "An int between 0 and 100 where 100 is full brightness.")) 
                (progDesc "Set brightness of the panels to a value between 0-100."))
        <> command "list-effects" 
            (info (pure ListEffects) (progDesc "List all downloaded effects on the panels."))
        <> command "show-selected-effect" 
            (info (pure GetSelectedEffect) (progDesc "Show currently selected effect."))
        <> command "set-selected-effect" 
            (info
                (argument (maybeReader (Just . SetSelectedEffect)) (metavar "STR" 
                    <> help "The name of the effect."))
                (progDesc "Show currently selected effect."))
        <> command "start-streaming" 
            (info 
                (StartNanoLeafExtCtrl <$> many (strArgument (metavar "STR"
                    <> help "A list of streaming effects(flash,wave,meter)")))
            (progDesc "Start streaming control data to the panels over a UDP socket based on system sound or microphone."))
        <> command "show-on-off" 
            (info (pure OnOffState) (progDesc "Check if panels are on or off.")))

parseInitialAuthToken :: ReadM (Maybe AuthToken)
parseInitialAuthToken = maybeReader (Just . Just . mkAuthToken . T.pack)
