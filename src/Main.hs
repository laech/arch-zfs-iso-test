module Main where

import qualified Data.Text as Text
import qualified VM

import Control.Exception
import Control.Monad
import Data.List
import System.Environment
import System.Exit
import System.Log.Logger
import VM (VM)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  args <- getArgs
  test args
  infoM "Main" "\nPASSED\n" >> exitSuccess

test :: [String] -> IO ()
test [iso] = do
  vm <- VM.build "arch-zfs-iso-test"
  VM.attachIso vm iso
  VM.run vm
  testZfsRepoKeyIsSigned vm
  testZfsIsInstalled vm
  VM.destroy vm
test _ = die "Usage: <this-program> <path-to-iso>"

testZfsRepoKeyIsSigned :: VM -> IO ()
testZfsRepoKeyIsSigned vm = do
  output <- VM.execute vm "pacman-key --list-keys F75D9D76"
  unless
    ("[  full  ] ArchZFS Bot <buildbot@archzfs.com>" `isInfixOf` output &&
     "[  full  ] ArchZFS Bot <bot@archzfs.com>" `isInfixOf` output)
    (throwIO (ExitFailure 1) <*
     errorM "Main" ("ArchZFS repo key not signed:\n" ++ output))

testZfsIsInstalled :: VM -> IO ()
testZfsIsInstalled vm = do
  output <- trim <$> VM.execute vm "zpool list"
  unless
    ("no pools available" == output)
    (throwIO (ExitFailure 1) <* errorM "Main" ("Unexpected output:\n" ++ output))

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack
