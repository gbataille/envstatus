module EnvStatus.Process (
  readProcessWithIgnoreExitCode
  , runCommandAsync
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))

import EnvStatus.Output.Parse (parseCommand)

readProcessWithIgnoreExitCode :: FilePath -> [String] -> String -> IO String
readProcessWithIgnoreExitCode command options stdin = do
  (exCode, stdout, _) <- readProcessWithExitCode command options stdin
  if exCode == ExitSuccess
    then return stdout
    else return ""

runCommandAsync :: String -> IO String
runCommandAsync cmd = do
  let components = parseCommand cmd
  mvCmd <- newEmptyMVar
  result <- readProcessWithIgnoreExitCode (head components) (tail components) ""
  _ <- forkIO $ putMVar mvCmd result
  takeMVar mvCmd
