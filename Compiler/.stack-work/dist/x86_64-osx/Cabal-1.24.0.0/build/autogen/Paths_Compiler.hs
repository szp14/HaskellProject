{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Compiler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/szp/Desktop/szp/haskellPro/HaskellProject/Compiler/.stack-work/install/x86_64-osx/nightly-2017-01-10/8.0.1/bin"
libdir     = "/Users/szp/Desktop/szp/haskellPro/HaskellProject/Compiler/.stack-work/install/x86_64-osx/nightly-2017-01-10/8.0.1/lib/x86_64-osx-ghc-8.0.1/Compiler-0.1.0.0-A5AqcFYQOeY4hOCoTusz4T"
datadir    = "/Users/szp/Desktop/szp/haskellPro/HaskellProject/Compiler/.stack-work/install/x86_64-osx/nightly-2017-01-10/8.0.1/share/x86_64-osx-ghc-8.0.1/Compiler-0.1.0.0"
libexecdir = "/Users/szp/Desktop/szp/haskellPro/HaskellProject/Compiler/.stack-work/install/x86_64-osx/nightly-2017-01-10/8.0.1/libexec"
sysconfdir = "/Users/szp/Desktop/szp/haskellPro/HaskellProject/Compiler/.stack-work/install/x86_64-osx/nightly-2017-01-10/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Compiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Compiler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Compiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Compiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Compiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
