{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Session.Backends.Memory
    ( initMemorySessionMananger
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (for_)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet (SnapletInit, makeSnaplet)
import           Snap.Snaplet.Session.Common (RNG, mkRNG, mkCSRFToken)
import           Snap.Snaplet.Session.SessionManager
                     ( SessionManager (SessionManager)
                     , ISessionManager
                     )
import qualified Snap.Snaplet.Session.SessionManager as S


-- snaplet-session-memory ----------------------------------------------------
import           Snap.Snaplet.MSession.Internal (Config, Session)
import qualified Snap.Snaplet.MSession.Internal as I


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


-- time ----------------------------------------------------------------------
import           Data.Time.Clock (NominalDiffTime)


-- unordered-containers ------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H


------------------------------------------------------------------------------
type Store = HashMap Text Text


------------------------------------------------------------------------------
data MemorySessionManager = MemorySessionManager
    { _cache :: !(Maybe (Session Payload))
    , _config :: !(Config Payload)
    , _timeout :: !NominalDiffTime
    , _rng :: !RNG
    }


------------------------------------------------------------------------------
data Payload = Payload
    { _csrf :: !Text
    , _store :: !Store
    }


------------------------------------------------------------------------------
smap :: (Session Payload -> Session Payload)
    -> MemorySessionManager -> MemorySessionManager
smap f (MemorySessionManager cache config timeout rng) =
    MemorySessionManager (fmap f cache) config timeout rng


------------------------------------------------------------------------------
pmap :: (Payload -> Payload) -> MemorySessionManager -> MemorySessionManager
pmap = smap . fmap


------------------------------------------------------------------------------
kvmap :: (Store -> Store) -> MemorySessionManager -> MemorySessionManager
kvmap f = pmap go
  where
    go (Payload csrf store) = Payload csrf $ f store


------------------------------------------------------------------------------
instance ISessionManager MemorySessionManager where
    load manager@(MemorySessionManager (Just _) _ _ _) = pure manager
    load (MemorySessionManager _ config timeout rng) = do
        session <- I.load config
        session' <- case I.get session of
            Nothing -> do
                csrf <- liftIO $ mkCSRFToken rng
                let payload = Payload csrf mempty
                pure $ I.renew timeout $ I.put payload session
            Just _ -> pure session
        pure $ MemorySessionManager (Just session') config timeout rng

    commit (MemorySessionManager cache config _ _) =
        for_ cache $ I.commit config

    reset manager@(MemorySessionManager cache config _ _) = do
        for_ cache $ I.commit config . I.reset
        pure manager

    touch manager = smap (I.renew (_timeout manager)) manager

    insert key = kvmap . H.insert key

    lookup key manager = _cache manager >>= I.get >>= H.lookup key . _store

    delete = kvmap . H.delete

    csrf manager = maybe err _csrf $ _cache manager >>= I.get
      where
        err = error "MemorySessionManager.csrf: This should never happen"

    toList manager = maybe [] (H.toList . _store) $ _cache manager >>= I.get


------------------------------------------------------------------------------
initMemorySessionMananger
    :: ByteString -- ^ Cookie name
    -> Maybe ByteString -- ^ Cookie domain
    -> Maybe NominalDiffTime -- ^ Cookie timeout
    -> SnapletInit b SessionManager
initMemorySessionMananger key domain mtimeout =
    makeSnaplet "MemorySession" description Nothing $ liftIO $ do
        rng <- liftIO mkRNG
        config <- liftIO $ I.init key domain
        pure $ SessionManager $ MemorySessionManager Nothing config timeout rng
  where
    timeout = maybe 86400 id mtimeout
    description = "A snaplet providing HTTP sessions backed by an IORef."
