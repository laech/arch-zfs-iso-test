module File
  ( removeFileIfExists
  ) where

import Control.Exception
import Control.Monad
import System.Directory
import System.IO.Error

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = removeFile file `catch` handle
  where
    handle e = unless (isDoesNotExistError e) (throwIO e)
