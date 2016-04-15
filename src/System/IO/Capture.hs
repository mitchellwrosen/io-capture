module System.IO.Capture where

import Control.Exception               (finally)
import System.Posix.IO
import System.Posix.Process.ByteString
import System.Posix.Types              (ProcessID)

import qualified Data.ByteString.Streaming as S

capture :: IO a -> IO (S.ByteString IO (), S.ByteString IO (), ProcessID)
capture act = do
  (out_r, out_w) <- createPipe
  (err_r, err_w) <- createPipe

  pid <- forkProcess $ do
    closeFd out_r
    closeFd err_r

    _ <- dupTo out_w stdOutput
    _ <- dupTo err_w stdError

    _ <- act `finally` (do
      closeFd out_w
      closeFd err_w)

    pure ()

  closeFd out_w
  closeFd err_w

  out_rh <- fdToHandle out_r
  err_rh <- fdToHandle err_r

  pure (S.hGetContents out_rh, S.hGetContents err_rh, pid)
