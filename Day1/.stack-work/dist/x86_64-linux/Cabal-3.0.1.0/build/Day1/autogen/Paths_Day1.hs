{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Day1 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/stoephu/dev/AoC2020/Day1/.stack-work/install/x86_64-linux/d12c6820080506cc76ef1ed62542fb40e5af47064a828737e37ed5e238c85fb5/8.8.4/bin"
libdir     = "/home/stoephu/dev/AoC2020/Day1/.stack-work/install/x86_64-linux/d12c6820080506cc76ef1ed62542fb40e5af47064a828737e37ed5e238c85fb5/8.8.4/lib/x86_64-linux-ghc-8.8.4/Day1-0.1.0.0-uw0dpL92QW48qOP6e2p2g-Day1"
dynlibdir  = "/home/stoephu/dev/AoC2020/Day1/.stack-work/install/x86_64-linux/d12c6820080506cc76ef1ed62542fb40e5af47064a828737e37ed5e238c85fb5/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/stoephu/dev/AoC2020/Day1/.stack-work/install/x86_64-linux/d12c6820080506cc76ef1ed62542fb40e5af47064a828737e37ed5e238c85fb5/8.8.4/share/x86_64-linux-ghc-8.8.4/Day1-0.1.0.0"
libexecdir = "/home/stoephu/dev/AoC2020/Day1/.stack-work/install/x86_64-linux/d12c6820080506cc76ef1ed62542fb40e5af47064a828737e37ed5e238c85fb5/8.8.4/libexec/x86_64-linux-ghc-8.8.4/Day1-0.1.0.0"
sysconfdir = "/home/stoephu/dev/AoC2020/Day1/.stack-work/install/x86_64-linux/d12c6820080506cc76ef1ed62542fb40e5af47064a828737e37ed5e238c85fb5/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Day1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Day1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Day1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Day1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Day1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Day1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
