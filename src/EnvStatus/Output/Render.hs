module EnvStatus.Output.Render where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.ConfigFile (ConfigParser)
import Data.Maybe (fromMaybe, isJust)

import EnvStatus.Config
import EnvStatus.Output.Types
import EnvStatus.Process

strip :: String -> String
strip s = reverse $ removeSpace $ reverse $ removeSpace s
  where
    removeSpace :: String -> String
    removeSpace (' ':rest) = removeSpace rest
    removeSpace ('\n':rest) = removeSpace rest
    removeSpace noSpaces = noSpaces

renderTokenString :: ConfigParser -> [Token] -> IO String
renderTokenString cp tokens = do
  results <- sequence $ renderToken cp <$> tokens
  return $ join results

renderToken :: ConfigParser -> Token -> IO String
renderToken _ (Raw s) = pure s
renderToken cp (SubCommand c) = do
  let cmd = getConfigValue cp $ strip c
  if isJust cmd
    then strip <$> runCommandAsync (fromMaybe "" cmd)
    else return ("### Command not found for config key " ++ c ++ " ###")
