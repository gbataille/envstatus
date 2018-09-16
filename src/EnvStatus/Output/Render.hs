module EnvStatus.Output.Render where

import Control.Applicative ((<$>))
import Data.ConfigFile (ConfigParser)
import Data.Maybe (fromMaybe)

import EnvStatus.Config
import EnvStatus.Output.Types

strip :: String -> String
strip s = reverse $ removeSpace $ reverse $ removeSpace s
  where
    removeSpace :: String -> String
    removeSpace (' ':rest) = removeSpace rest
    removeSpace noSpaces = noSpaces

renderTokenString :: ConfigParser -> [Token] -> String
renderTokenString cp tokens = concat $ renderToken cp <$> tokens

renderToken :: ConfigParser -> Token -> String
renderToken _ (Raw s) = "R(" ++ s ++ ")"
renderToken cp (SubCommand c) =
  fromMaybe (" #!# Not Configured: " ++ sc ++ " #!# ") $ getConfigValue cp sc
    where
      sc = strip c
