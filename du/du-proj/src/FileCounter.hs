module FileCounter (fileCount) where

import App
import System.Directory.Extra (listFiles)
import Utils

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
  AppEnv {cfg, path, depth, fileStatus} <- ask
  fs <- currentPathStatus
  when (isDirectory fs && depth <= maxDepth cfg) $ do
    traverseDirectoryWith fileCount
    files <- liftIO $ listFiles path
    tell [(path, length $ filter (checkExtension cfg) files)]