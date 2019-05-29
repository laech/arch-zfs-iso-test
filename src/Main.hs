module Main where

import qualified Data.Text as Text
import qualified VM

import Control.Exception
import Control.Monad
import System.Environment
import System.Exit
import System.Log.Logger
import VM (VM)

main :: IO ()
main =
  getArgs >>= run >>= \passed ->
    if passed
      then infoM "Main" "\nPASSED\n" >> exitSuccess
      else infoM "Main" "\nFAILED\n" >> exitFailure

run :: [String] -> IO Bool
run [zfsVersion, iso] = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  vm <- VM.build "arch-zfs-iso-test"
  result <- test vm zfsVersion iso
  VM.destroy vm
  pure result
run _ = die "Usage: <this-program> <zfs-version> <path-to-iso>"

test :: VM -> String -> FilePath -> IO Bool
test vm zfsVersion iso = do
  VM.attachIso vm iso
  VM.run vm
  kernelVersion <- trim <$> VM.execute vm "uname -r"
  actualStatus <- trim <$> VM.execute vm "dkms status"
  let expectedStatus =
        "zfs, " ++ zfsVersion ++ ", " ++ kernelVersion ++ ", x86_64: installed"
  if expectedStatus == actualStatus
    then pure True
    else do
      infoM "Main" ("Expected:\n" ++ expectedStatus)
      infoM "Main" ("Actual:\n" ++ actualStatus)
      pure False

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack
