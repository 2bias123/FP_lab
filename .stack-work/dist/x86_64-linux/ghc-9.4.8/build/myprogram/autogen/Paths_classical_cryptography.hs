{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_classical_cryptography (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/mnt/c/hub/suli/egyetem/4/P4/FP/Project/FP_lab/.stack-work/install/x86_64-linux/4b9ed267e98645febb4d846723b1c9459a3d01a9c78201d757a33098ccc53e6a/9.4.8/bin"
libdir     = "/mnt/c/hub/suli/egyetem/4/P4/FP/Project/FP_lab/.stack-work/install/x86_64-linux/4b9ed267e98645febb4d846723b1c9459a3d01a9c78201d757a33098ccc53e6a/9.4.8/lib/x86_64-linux-ghc-9.4.8/classical-cryptography-0.1.0.0-B2lYcygaEsDHadoh1lgCmR-myprogram"
dynlibdir  = "/mnt/c/hub/suli/egyetem/4/P4/FP/Project/FP_lab/.stack-work/install/x86_64-linux/4b9ed267e98645febb4d846723b1c9459a3d01a9c78201d757a33098ccc53e6a/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/mnt/c/hub/suli/egyetem/4/P4/FP/Project/FP_lab/.stack-work/install/x86_64-linux/4b9ed267e98645febb4d846723b1c9459a3d01a9c78201d757a33098ccc53e6a/9.4.8/share/x86_64-linux-ghc-9.4.8/classical-cryptography-0.1.0.0"
libexecdir = "/mnt/c/hub/suli/egyetem/4/P4/FP/Project/FP_lab/.stack-work/install/x86_64-linux/4b9ed267e98645febb4d846723b1c9459a3d01a9c78201d757a33098ccc53e6a/9.4.8/libexec/x86_64-linux-ghc-9.4.8/classical-cryptography-0.1.0.0"
sysconfdir = "/mnt/c/hub/suli/egyetem/4/P4/FP/Project/FP_lab/.stack-work/install/x86_64-linux/4b9ed267e98645febb4d846723b1c9459a3d01a9c78201d757a33098ccc53e6a/9.4.8/etc"

getBinDir     = catchIO (getEnv "classical_cryptography_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "classical_cryptography_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "classical_cryptography_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "classical_cryptography_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "classical_cryptography_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "classical_cryptography_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
