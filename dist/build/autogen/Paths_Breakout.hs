module Paths_Breakout (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chase/.cabal/bin"
libdir     = "/home/chase/.cabal/lib/x86_64-linux-ghc-7.8.3/Breakout-0.1.0.0"
datadir    = "/home/chase/.cabal/share/x86_64-linux-ghc-7.8.3/Breakout-0.1.0.0"
libexecdir = "/home/chase/.cabal/libexec"
sysconfdir = "/home/chase/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Breakout_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Breakout_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Breakout_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Breakout_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Breakout_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
