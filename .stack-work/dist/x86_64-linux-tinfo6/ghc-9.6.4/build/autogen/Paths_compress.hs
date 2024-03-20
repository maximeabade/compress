{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_compress (
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
bindir     = "/home/max/Desktop/Work/compress/.stack-work/install/x86_64-linux-tinfo6/a476bebc6d17c061ef713e62ba337983d9b60e643237ae98b03e4d1487502446/9.6.4/bin"
libdir     = "/home/max/Desktop/Work/compress/.stack-work/install/x86_64-linux-tinfo6/a476bebc6d17c061ef713e62ba337983d9b60e643237ae98b03e4d1487502446/9.6.4/lib/x86_64-linux-ghc-9.6.4/compress-0.1.0.0-3OvsreSP05Q70yje1Lhasf"
dynlibdir  = "/home/max/Desktop/Work/compress/.stack-work/install/x86_64-linux-tinfo6/a476bebc6d17c061ef713e62ba337983d9b60e643237ae98b03e4d1487502446/9.6.4/lib/x86_64-linux-ghc-9.6.4"
datadir    = "/home/max/Desktop/Work/compress/.stack-work/install/x86_64-linux-tinfo6/a476bebc6d17c061ef713e62ba337983d9b60e643237ae98b03e4d1487502446/9.6.4/share/x86_64-linux-ghc-9.6.4/compress-0.1.0.0"
libexecdir = "/home/max/Desktop/Work/compress/.stack-work/install/x86_64-linux-tinfo6/a476bebc6d17c061ef713e62ba337983d9b60e643237ae98b03e4d1487502446/9.6.4/libexec/x86_64-linux-ghc-9.6.4/compress-0.1.0.0"
sysconfdir = "/home/max/Desktop/Work/compress/.stack-work/install/x86_64-linux-tinfo6/a476bebc6d17c061ef713e62ba337983d9b60e643237ae98b03e4d1487502446/9.6.4/etc"

getBinDir     = catchIO (getEnv "compress_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "compress_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "compress_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "compress_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "compress_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "compress_sysconfdir") (\_ -> return sysconfdir)



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
