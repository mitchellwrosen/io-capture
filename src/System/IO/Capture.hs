{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Capture
  ( capture
  , captureStream
  ) where

import Control.Exception
import Control.Monad
import System.IO            (hClose, hPutStr)
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types   (ProcessID)

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Streaming as S

-- For haddocks
import Data.ByteString.Streaming (stdout)

-- | Capture an IO action's @stdout@, @stderr@, and any thrown exception. Waits
-- for the action to complete before returning.
--
-- @
-- > (out, _, _, _) <- 'capture' ('putStrLn' "foo")
-- > 'print' out
-- \"foo\"
--
-- > (_, err, _, _) <- 'capture' ('System.IO.hPutStrLn' 'System.IO.stderr' \"bar\")
-- > 'print' err
-- \"bar\"
--
-- > (_, _, exc, _) <- 'capture' 'undefined'
-- > 'print' exc
-- \"Prelude.undefined\"
-- @
capture :: IO a -> IO (LBS.ByteString, LBS.ByteString, LBS.ByteString, Maybe ProcessStatus)
capture act = do
  (out, err, exc, pid) <- captureStream act
  status <- getProcessStatus True True pid
  (,,,)
    <$> S.toLazy_ out
    <*> S.toLazy_ err
    <*> S.toLazy_ exc
    <*> pure status

-- | Stream an IO action's @stdout@, @stderr@, and any thrown exception. Also
-- returns the spawned process's pid to @wait@ on (otherwise the process will
-- become a zombie after terminating), kill, etc.
--
-- @
-- import qualified Data.ByteString.Streaming as S
--
-- > let action = 'putStrLn' \"foo\" >> 'Control.Concurrent.threadDelay' 1000000
-- > (out, _, _, pid) <- 'captureStream' ('Control.Monad.replicateM_' 5 action)
-- > S.'stdout' out
-- foo
-- foo
-- foo
-- foo
-- foo
-- > 'getProcessStatus' True True pid
-- Just ('Exited' 'System.Exit.ExitSuccess')
-- @
captureStream
  :: IO a
  -> IO ( S.ByteString IO ()
        , S.ByteString IO ()
        , S.ByteString IO ()
        , ProcessID
        )
captureStream act = do
  (out_r, out_w) <- createPipe
  (err_r, err_w) <- createPipe
  (exc_r, exc_w) <- createPipe

  pid <- forkProcess $ do
    closeFd out_r
    closeFd err_r
    closeFd exc_r

    _ <- dupTo out_w stdOutput
    _ <- dupTo err_w stdError

    exc_wh <- fdToHandle exc_w

    void act `catch` \(e :: SomeException) -> hPutStr exc_wh (show e)

    closeFd out_w
    closeFd err_w
    hClose exc_wh

  closeFd out_w
  closeFd err_w
  closeFd exc_w

  out_rh <- fdToHandle out_r
  err_rh <- fdToHandle err_r
  exc_rh <- fdToHandle exc_r

  pure ( S.hGetContents out_rh
       , S.hGetContents err_rh
       , S.hGetContents exc_rh
       , pid
       )
