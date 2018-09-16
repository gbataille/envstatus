module EnvStatus.Output.Types where

data OutputFormat = ZSH | BASH | TMUX | NONE | Other deriving (Show)
instance Read OutputFormat where
  readsPrec _ input =
    case input of
        "zsh" -> return (ZSH, "")
        "bash" -> return (BASH, "")
        "tmux" -> return (TMUX, "")
        "none" -> return (NONE, "")
        _ -> return (Other, "")

data Token = Raw String | SubCommand String deriving (Show)
