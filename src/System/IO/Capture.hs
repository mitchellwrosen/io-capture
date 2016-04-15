{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Capture (capture) where

import Control.Exception
import Control.Monad
import System.Posix.IO
import System.Posix.Process.ByteString
import System.Posix.Types              (ByteCount, Fd, ProcessID)

import qualified Data.ByteString.Streaming as S

-- For haddocks
import Data.ByteString.Streaming (toLazy_)

-- | Capture an IO action's @stdout@, @stderr@, and any thrown exception. Also
-- returns the pid of the spawned child.
--
-- @
-- import System.IO
-- import qualified Streaming.ByteString as S
--
-- > (out, _, _, _) <- capture ('putStrLn' "foo")
-- > S.'toLazy_' out
-- \"foo\"
--
-- > (_, err, _, _) <- capture ('System.IO.hPutStrLn' 'System.IO.stderr' "bar")
-- > S.'toLazy_' err
-- \"bar\"
--
-- > (_, _, exc, _) <- capture 'undefined'
-- > S.'toLazy_' exc
-- \"Prelude.undefined\"
-- @
capture
  :: IO a
  -> IO ( S.ByteString IO ()
        , S.ByteString IO ()
        , S.ByteString IO ()
        , ProcessID
        )
capture act = do
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

  pure (S.hGetContents out_rh, S.hGetContents err_rh, S.hGetContents exc_rh, pid)

fdWriteAll :: Fd -> String -> IO ()
fdWriteAll fd str0 = go str0 (fromIntegral (length str0))
 where
  go :: String -> ByteCount -> IO ()
  go str n = do
    n' <- fdWrite fd str
    when (n' < n) $
      go (drop (fromIntegral n') str) (n - n')
