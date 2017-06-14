module Main where


import qualified Data.Text          as Text
import qualified VM


import           Control.Exception  (finally)
import           System.Environment (getArgs)
import           System.Exit        (die, exitFailure, exitSuccess)
import           VM                 (VM)


main :: IO ()
main = getArgs >>= run >>= \passed -> if passed
  then putStrLn "\nPASSED\n" >> exitSuccess
  else putStrLn "\nFAILED\n" >> exitFailure


run :: [String] -> IO Bool
run [zfsVersion, iso] =
  VM.build "arch-zfs-iso-test" >>= \vm ->
  test vm zfsVersion iso `finally` VM.destroy vm

run _ = die "Usage: <this-program> <zfs-version> <path-to-iso>"


test :: VM -> String -> FilePath -> IO Bool
test vm zfsVersion iso = do
  VM.attachIso vm iso
  VM.run vm

  kernelVersion <- trim <$> VM.execute vm "uname -r"
  actualStatus  <- trim <$> VM.execute vm "dkms status"

  let expectedStatus =
        "spl, "++zfsVersion++", "++kernelVersion++", x86_64: installed\n"++
        "zfs, "++zfsVersion++", "++kernelVersion++", x86_64: installed"

  if expectedStatus == actualStatus
    then pure True
    else do
      putStrLn ("Expected:\n" ++ expectedStatus)
      putStrLn ("Actual:\n" ++ actualStatus)
      pure False


trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack
