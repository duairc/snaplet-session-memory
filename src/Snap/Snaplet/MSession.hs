{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.MSession
    ( MSession, initMSession
    , getSession, putSession, setSession, renewSession, resetSession
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- mtl -----------------------------------------------------------------------
import           Control.Monad.Reader.Class (ask, asks)
import           Control.Monad.State.Class (put)


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet (Handler, SnapletInit, makeSnaplet)


-- snap-core -----------------------------------------------------------------
import           Snap.Core (liftSnap)


-- snaplet-session-memory ----------------------------------------------------
import           Snap.Snaplet.MSession.Internal (Session, Config)
import qualified Snap.Snaplet.MSession.Internal as I


-- time ----------------------------------------------------------------------
import           Data.Time.Clock (NominalDiffTime)


------------------------------------------------------------------------------
data MSession a = MSession
    { _cache :: !(Maybe (Session a))
    , _config :: !(Config a)
    }


------------------------------------------------------------------------------
initMSession :: ByteString -> Maybe ByteString -> Maybe ByteString
    -> SnapletInit b (MSession a)
initMSession key domain path = makeSnaplet "MSession" description Nothing $ do
    config <- liftIO $ I.init key domain path
    pure $ MSession Nothing config
  where
    description = "A snaplet providing HTTP sessions backed by an IORef."


------------------------------------------------------------------------------
loadSession :: Handler b (MSession a) (Session a)
loadSession = do
    MSession cache config <- ask
    case cache of
        Nothing -> do
            session <- liftSnap $ I.load config
            put $ MSession (Just session) config
            pure session
        Just session -> pure session


------------------------------------------------------------------------------
withSession :: (Session a -> Session a) -> Handler b (MSession a) ()
withSession f = do
    session <- loadSession
    config <- asks _config
    let session' = f session
    put $ MSession (Just session) config
    liftSnap $ I.commit config session'


------------------------------------------------------------------------------
getSession :: Handler b (MSession a) (Maybe a)
getSession = I.get <$> loadSession


------------------------------------------------------------------------------
putSession :: a -> NominalDiffTime -> Handler b (MSession a) ()
putSession a timeout = withSession (I.renew timeout . I.put a . I.reset)


------------------------------------------------------------------------------
setSession :: a -> Handler b (MSession a) ()
setSession = withSession . I.modify . const


------------------------------------------------------------------------------
renewSession :: NominalDiffTime -> Handler b (MSession a) ()
renewSession = withSession . I.renew


------------------------------------------------------------------------------
resetSession :: Handler b (MSession a) ()
resetSession = withSession I.reset
