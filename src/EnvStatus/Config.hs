module EnvStatus.Config where

import System.Posix.Files (fileExist)
import System.Posix.User

import Data.ConfigFile (ConfigParser, readstring, emptyCP, get, set, merge, to_string)

readConfig :: String -> IO ConfigParser
readConfig configString =
  case parsed of
    Right configParser -> return configParser
    Left cperror -> do
      print cperror
      return emptyCP
    where
      parsed = readstring emptyCP configString

getConfigValue :: ConfigParser -> String -> Maybe String
getConfigValue cp key =
  case value of
      Right v -> Just v
      Left _error -> Nothing
      where value = get cp "DEFAULT" key

defaultConfig :: ConfigParser
defaultConfig =
  case defaultC of
      Right config -> config
      Left _error -> emptyCP
      where
        -- TODO: gbataille - change the default to something reasonable
        defaultC = do
          cp <- set emptyCP "DEFAULT" "output_template" "\nenvstatus: {{foo}}"
          set cp "DEFAULT" "foo" "date +%Y-%m-%d"

getAppConfig :: IO ConfigParser
getAppConfig = do
  userEntry <- getRealUserID >>= getUserEntryForID
  let configFilePath = homeDirectory userEntry ++ "/.envstatusrc"
  configFilePresent <- fileExist configFilePath
  if configFilePresent
    then do
      configText <- readFile configFilePath
      cp <- readConfig configText
      return $ merge defaultConfig cp
    else return defaultConfig

-- | For debug purpose
showConfig :: ConfigParser -> String
showConfig = to_string
