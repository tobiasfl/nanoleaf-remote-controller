module CommandLine.CommandLine 
    ( requestParser
    ) where

import Options.Applicative
import CommandLine.Types
import Types (AuthToken, mkAuthToken, Request (..), Command (..))
import Data.Text as T
import Text.Read (readMaybe)

requestParser:: Parser Request
requestParser = Request
      <$> commandParser
      <*> option parseInitialAuthToken
          ( long "authToken"
         <> short 'a'
         <> help "An existing authentication token to use for performing nanoleaf actions."
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
        <> command "show-on-off" 
            (info (pure OnOffState) (progDesc "Check if panels are on or off.")))

parseInitialAuthToken :: ReadM (Maybe AuthToken)
parseInitialAuthToken = maybeReader (Just . Just . mkAuthToken . T.pack)

