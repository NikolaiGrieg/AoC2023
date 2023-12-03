{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_x3 (
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

bindir     = "/home/nikolai/.cabal/bin"
libdir     = "/home/nikolai/.cabal/lib/x86_64-linux-ghc-8.8.4/x3-0.1.0.0-inplace-x3"
dynlibdir  = "/home/nikolai/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/nikolai/.cabal/share/x86_64-linux-ghc-8.8.4/x3-0.1.0.0"
libexecdir = "/home/nikolai/.cabal/libexec/x86_64-linux-ghc-8.8.4/x3-0.1.0.0"
sysconfdir = "/home/nikolai/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "x3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "x3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "x3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "x3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "x3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "x3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
