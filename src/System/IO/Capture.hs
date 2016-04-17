{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Capture
  ( capture
  , captureStream
  ) where

import Control.Exception
import Control.Monad
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types (ByteCount, Fd)

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
  (out, err, exc, waitpid) <- captureStream act
  status <- waitpid
  (,,,)
    <$> S.toLazy_ out
    <*> S.toLazy_ err
    <*> S.toLazy_ exc
    <*> pure status

-- | Stream an IO action's @stdout@, @stderr@, and any thrown exception. Also
-- returns an action that @wait@s on the spawn child. This must be called
-- eventually, otherwise the child will remain a zombie.
--
-- @
-- import qualified Data.ByteString.Streaming as S
--
-- > let action = 'putStrLn' \"foo\" >> 'Control.Concurrent.threadDelay' 1000000
-- > (out, _, _, waitpid) <- 'captureStream' ('Control.Monad.replicateM_' 5 action)
-- > S.'stdout' out
-- foo
-- foo
-- foo
-- foo
-- foo
-- > waitpid
-- Just ('Exited' 'System.Exit.ExitSuccess')
-- @
captureStream
  :: IO a
  -> IO ( S.ByteString IO ()
        , S.ByteString IO ()
        , S.ByteString IO ()
        , IO (Maybe ProcessStatus)
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

    void act `catch` \(e :: SomeException) -> fdWriteAll exc_w (show e)

    closeFd out_w
    closeFd err_w
    closeFd exc_w

  closeFd out_w
  closeFd err_w
  closeFd exc_w

  out_rh <- fdToHandle out_r
  err_rh <- fdToHandle err_r
  exc_rh <- fdToHandle exc_r

  pure ( S.hGetContents out_rh
       , S.hGetContents err_rh
       , S.hGetContents exc_rh
       , getProcessStatus True True pid
       )

fdWriteAll :: Fd -> String -> IO ()
fdWriteAll fd str0 = go str0 (fromIntegral (length str0))
 where
  go :: String -> ByteCount -> IO ()
  go str n = do
    n' <- fdWrite fd str
    when (n' < n) $
      go (drop (fromIntegral n') str) (n - n')
