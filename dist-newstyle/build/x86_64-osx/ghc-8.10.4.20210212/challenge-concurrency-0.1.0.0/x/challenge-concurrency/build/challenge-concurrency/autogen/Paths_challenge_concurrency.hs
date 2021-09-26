{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_challenge_concurrency (
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

bindir     = "/Users/noahjones/.cabal/bin"
libdir     = "/Users/noahjones/.cabal/lib/x86_64-osx-ghc-8.10.4.20210212/challenge-concurrency-0.1.0.0-inplace-challenge-concurrency"
dynlibdir  = "/Users/noahjones/.cabal/lib/x86_64-osx-ghc-8.10.4.20210212"
datadir    = "/Users/noahjones/.cabal/share/x86_64-osx-ghc-8.10.4.20210212/challenge-concurrency-0.1.0.0"
libexecdir = "/Users/noahjones/.cabal/libexec/x86_64-osx-ghc-8.10.4.20210212/challenge-concurrency-0.1.0.0"
sysconfdir = "/Users/noahjones/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "challenge_concurrency_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "challenge_concurrency_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "challenge_concurrency_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "challenge_concurrency_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "challenge_concurrency_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "challenge_concurrency_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
