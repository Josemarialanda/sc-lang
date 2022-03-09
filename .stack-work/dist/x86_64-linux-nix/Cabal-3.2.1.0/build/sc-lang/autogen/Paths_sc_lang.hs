{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sc_lang (
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

bindir     = "/home/jose/Documents/code/sc-lang/.stack-work/install/x86_64-linux-nix/da3eba0b3fc1b23dc977ac6bd517320c6520d26d280ae3533edf4d1568efa9b6/8.10.7/bin"
libdir     = "/home/jose/Documents/code/sc-lang/.stack-work/install/x86_64-linux-nix/da3eba0b3fc1b23dc977ac6bd517320c6520d26d280ae3533edf4d1568efa9b6/8.10.7/lib/x86_64-linux-ghc-8.10.7/sc-lang-0.1.0.0-CRqshPmL16Q8NBpJ3p2xLl-sc-lang"
dynlibdir  = "/home/jose/Documents/code/sc-lang/.stack-work/install/x86_64-linux-nix/da3eba0b3fc1b23dc977ac6bd517320c6520d26d280ae3533edf4d1568efa9b6/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/jose/Documents/code/sc-lang/.stack-work/install/x86_64-linux-nix/da3eba0b3fc1b23dc977ac6bd517320c6520d26d280ae3533edf4d1568efa9b6/8.10.7/share/x86_64-linux-ghc-8.10.7/sc-lang-0.1.0.0"
libexecdir = "/home/jose/Documents/code/sc-lang/.stack-work/install/x86_64-linux-nix/da3eba0b3fc1b23dc977ac6bd517320c6520d26d280ae3533edf4d1568efa9b6/8.10.7/libexec/x86_64-linux-ghc-8.10.7/sc-lang-0.1.0.0"
sysconfdir = "/home/jose/Documents/code/sc-lang/.stack-work/install/x86_64-linux-nix/da3eba0b3fc1b23dc977ac6bd517320c6520d26d280ae3533edf4d1568efa9b6/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sc_lang_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sc_lang_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sc_lang_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sc_lang_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sc_lang_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sc_lang_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
