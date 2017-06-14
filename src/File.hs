module File (removeFileIfExists) where


import           Control.Exception  (catch, throwIO)
import           Control.Monad      (unless)
import           System.Directory   (removeFile)
import           System.IO.Error    (isDoesNotExistError)


removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = removeFile file `catch` handle
  where handle e = unless (isDoesNotExistError e) (throwIO e)
