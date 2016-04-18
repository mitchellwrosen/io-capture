{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import System.IO.Capture

import Control.Concurrent
import Control.Monad
import System.IO
import System.Posix.Process
import System.Posix.Signals
import System.Timeout
import Test.Hspec
import Test.Hspec.Core.Runner

import qualified Data.ByteString.Streaming as S

main :: IO ()
main = hspecWith config $ do
  describe "capture" $ do
    it "captures everything" $ do
      (out, err, exc, _) <- capture (putStr "foo" >> hPutStr stderr "bar" >> undefined)
      out `shouldBe` "foo"
      err `shouldBe` "bar"
      exc `shouldBe` "Prelude.undefined"

  describe "captureStream" $ do
    it "streams output" $ do
      (out, _, _, pid) <- captureStream (forever (putStrLn "foo" >> threadDelay (1*sec)))

      timeout (2*sec) (S.uncons out) >>= \case
        Just (Just _) -> do
          signalProcess sigKILL pid
          _ <- getProcessStatus True True pid
          pure ()
        _ -> expectationFailure "expected a byte, found nothing"


 where
  -- hspec in color mode messes with stdout
  config = defaultConfig
    { configColorMode = ColorNever }

sec :: Int
sec = 1000000
