module VM
  ( VM
  , build
  , run
  , attachIso
  , execute
  , destroy
  ) where

import qualified Scancode

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Functor
import Data.List (isInfixOf)
import Data.Typeable
import File
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Log.Logger
import System.Process

data TimeoutException =
  TimeoutException
  deriving (Show, Typeable)

instance Exception TimeoutException

data VM = VM
  { name :: String
  , directory :: FilePath
  , privateKey :: FilePath
  , publicKey :: FilePath
  } deriving (Show)

sshIp = "127.0.0.1"

sshPort = "22222"

build :: String -> IO VM
build name = doCreate `catch` doCleanup
  where
    dir = getTemporaryDirectory >>= \dir -> createTempDirectory dir "testXXX"
    doCreate :: IO VM
    doCreate = do
      deleteIfExists name
      create name
      configRam name
      configHdd name =<< dir
      configSshPortForwarding name
      dir >>= \dir ->
        let privateKey = (dir </> "id_test")
            publicKey = (dir </> "id_test.pub")
         in pure (VM name dir privateKey publicKey)
    doCleanup :: IOException -> IO VM
    doCleanup e = do
      deleteIfExists name
      removeDirectoryRecursive =<< dir
      throwIO e

destroy :: VM -> IO ()
destroy vm = do
  deleteIfExists (name vm)
  removeFileIfExists (privateKey vm)
  removeFileIfExists (publicKey vm)
  removeDirectoryRecursive (directory vm)

run :: VM -> IO ()
run vm = do
  genSshKeyPair (privateKey vm) (publicKey vm)
  pubKeyStr <- readFile (publicKey vm)
  start (name vm)
  threadDelay 10000000
  sendKeys (name vm) ""
  waitForStarted (name vm)
  sendKeys (name vm) "mkdir -p ~/.ssh"
  sendKeys (name vm) ("echo '" ++ pubKeyStr ++ "' > ~/.ssh/authorized_keys")
  sendKeys (name vm) "systemctl start sshd"
  -- Wait for SSH to be ready
  threadDelay 6000000

waitForStarted :: String -> IO ()
waitForStarted vm = waitForStarted' 0
  where
    waitForStarted' tries = do
      when (tries >= 30) $ throwIO TimeoutException
      infoM "VM" $ "Booting (" ++ show tries ++ ")..."
      log <- vboxmanageRead' False ["showvminfo", vm, "--log", "0"]
      let okay = any (isInfixOf "DHCP offered IP address") (lines log)
      -- Wait for rest to initialize, even when log entry found
      threadDelay 15000000
      if okay
        then pure ()
        else waitForStarted' (tries + 1)

attachIso :: VM -> FilePath -> IO ()
attachIso vm iso = add >> attach
  where
    add = vboxmanage ["storagectl", name vm, "--name", "IDE", "--add", "ide"]
    attach =
      vboxmanage
        [ "storageattach"
        , name vm
        , "--storagectl"
        , "IDE"
        , "--port"
        , "0"
        , "--device"
        , "0"
        , "--type"
        , "dvddrive"
        , "--medium"
        , iso
        ]

sendKeys :: String -> String -> IO ()
sendKeys vm command = mapM_ send (command ++ "\n")
  where
    send char =
      vboxmanage (["controlvm", vm, "keyboardputscancode"] ++ Scancode.get char)

execute :: VM -> String -> IO String
execute vm command = do
  infoM "VM" $ "Executing: " ++ command
  output <-
    readProcess
      "ssh"
      [ "-q"
      , "-i"
      , privateKey vm
      , "-o"
      , "PasswordAuthentication=no"
      , "-o"
      , "UserKnownHostsFile=/dev/null"
      , "-o"
      , "StrictHostKeyChecking=no"
      , "-p"
      , sshPort
      , "root@" ++ sshIp
      , command
      ]
      ""
  unless (null output) $ infoM "VM" $ "Output: " ++ output
  pure output

genSshKeyPair :: FilePath -> FilePath -> IO ()
genSshKeyPair privateKey publicKey = do
  removeFileIfExists privateKey
  removeFileIfExists publicKey
  callProcess "ssh-keygen" ["-t", "ed25519", "-f", privateKey, "-N", "", "-q"]

create :: String -> IO ()
create vm =
  vboxmanage
    ["createvm", "--name", vm, "--ostype", "ArchLinux_64", "--register"]

configRam :: String -> IO ()
configRam vm = vboxmanage ["modifyvm", vm, "--memory", "512"]

configSshPortForwarding :: String -> IO ()
configSshPortForwarding vm =
  vboxmanage
    [ "modifyvm"
    , vm
    , "--natpf1"
    , "guestssh,tcp," ++ sshIp ++ "," ++ sshPort ++ ",,22"
    ]

configHdd :: String -> FilePath -> IO ()
configHdd vm dir = add >> create >> attach
  where
    medium = dir </> (vm ++ ".vdi")
    add = vboxmanage ["storagectl", vm, "--name", "SATA", "--add", "sata"]
    create =
      vboxmanage
        [ "createmedium"
        , "disk"
        , "--filename"
        , medium
        , "--format"
        , "VDI"
        , "--size"
        , "1024"
        ]
    attach =
      vboxmanage
        [ "storageattach"
        , vm
        , "--storagectl"
        , "SATA"
        , "--port"
        , "0"
        , "--device"
        , "0"
        , "--type"
        , "hdd"
        , "--medium"
        , medium
        ]

start :: String -> IO ()
start vm = vboxmanage ["startvm", vm]

deleteIfExists :: String -> IO ()
deleteIfExists vm = stop >> remove
  where
    stop = listRunning >>= \vms -> when (vm `elem` vms) (poweroffWait vm)
    remove = list >>= \vms -> when (vm `elem` vms) (delete vm)

poweroffWait :: String -> IO ()
poweroffWait vm = poweroff vm >> threadDelay 2000000 -- Wait for unlock

poweroff :: String -> IO ()
poweroff vm = vboxmanage ["controlvm", vm, "poweroff"]

delete :: String -> IO ()
delete vm = vboxmanage ["unregistervm", vm, "--delete"]

list :: IO [String]
list = parseNames <$> vboxmanageRead ["list", "vms"]

listRunning :: IO [String]
listRunning = parseNames <$> vboxmanageRead ["list", "runningvms"]

parseNames :: String -> [String]
parseNames = map parseName . filter (not . null) . lines

-- Example output of listing VMs:
-- "default" {f052aee7-9dab-44ca-9797-f2e429a72ce0}
-- "Arch" {f6a2a54c-d485-4cf2-929e-82f4594aca0a}
parseName :: String -> String
parseName line = name
  where
    part = dropWhile (/= ' ') . reverse $ line
    name = drop 1 . reverse . drop 2 $ part

vboxmanage :: [String] -> IO ()
vboxmanage args = vboxmanageRead args $> ()

vboxmanageRead :: [String] -> IO String
vboxmanageRead = vboxmanageRead' True

vboxmanageRead' :: Bool -> [String] -> IO String
vboxmanageRead' doOutput args = do
  infoM "VM" $ foldMap (++ " ") ("Executing: vboxmanage" : args)
  output <- readProcess "vboxmanage" args ""
  when (doOutput && not (null output)) $ infoM "VM" $ "Output: " ++ output
  pure output
