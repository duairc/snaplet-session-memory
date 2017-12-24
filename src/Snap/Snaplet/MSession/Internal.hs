{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Snap.Snaplet.MSession.Internal
    ( Config, Session, init, load, commit, reset, renew, get, put, modify
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (empty)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forever, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (for_)
import           Data.IORef (IORef, newIORef, atomicModifyIORef')
import           Data.List (partition)
import           Data.Traversable (for)
import           Prelude hiding (init)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- snap-core -----------------------------------------------------------------
import           Snap.Core
                     ( Snap
                     , Cookie (Cookie), cookieName, cookieValue, getCookie
                     , expireCookie
                     , getsRequest, modifyRequest, rqCookies
                     , modifyResponse, addResponseCookie
                     )


-- snaplet-session-memory ----------------------------------------------------
import           Data.TimeoutMap (TimeoutMap)
import qualified Data.TimeoutMap as TO


-- time ----------------------------------------------------------------------
import           Data.Time.Clock
                     ( NominalDiffTime, addUTCTime
                     , UTCTime, getCurrentTime
                     )


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)


-- uuid ----------------------------------------------------------------------
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.UUID.V4 (nextRandom)


------------------------------------------------------------------------------
data Config a = Config
    { _cookie :: !ByteString
    , _domain :: !(Maybe ByteString)
    , _sessions :: !(IORef (TimeoutMap UUID a))
    }


------------------------------------------------------------------------------
data Session a = Session
    { _now :: !UTCTime
    , _uuid :: !(Maybe UUID)
    , _timeout :: !(Maybe NominalDiffTime)
    , _payload :: !(Maybe a)
    }
  deriving (Functor)


------------------------------------------------------------------------------
init :: ByteString -> Maybe ByteString -> IO (Config a)
init key domain = do
    sessions <- newIORef mempty
    _ <- forkIO (go sessions)
    pure $ Config key domain sessions
  where
    go sessions = forever $ do
        threadDelay 600000000
        now <- getCurrentTime
        atomicModifyIORef' sessions $ (, ()) . TO.clean now


------------------------------------------------------------------------------
load :: Config a -> Snap (Session a)
load (Config key domain sessions) = do
    now <- liftIO getCurrentTime
    muuid <- viewCookie key
    case muuid of
        Nothing -> pure $ Session now Nothing Nothing Nothing
        Just uuid -> do
            mpayload <- liftIO $ atomicModifyIORef' sessions $
                TO.lookup uuid now
            case mpayload of
                Nothing -> do
                    unsetCookie key domain
                    pure $ Session now Nothing Nothing Nothing
                Just _ -> pure $ Session now muuid Nothing mpayload


------------------------------------------------------------------------------
commit :: Config a -> Session a -> Snap ()
commit config session = go mpayload muuid
  where
    Session now muuid mtimeout mpayload = session
    Config key domain sessions = config
    go Nothing Nothing = pure ()
    go Nothing (Just uuid) = do
        liftIO $ atomicModifyIORef' sessions $ (, ()) . TO.delete uuid now
        unsetCookie key domain
    go (Just payload) Nothing = do
        uuid <- liftIO nextRandom
        expiry <- liftIO $ atomicModifyIORef' sessions $
            TO.insert payload timeout uuid now
        setCookie key domain uuid expiry
      where
        timeout = maybe 86400 id mtimeout
    go (Just payload) (Just uuid) = do
        mexpiry <- liftIO $ atomicModifyIORef' sessions $
            TO.adjust (\_ -> (payload, mtimeout)) uuid now
        for_ mexpiry $ \expiry -> for mtimeout $ \timeout ->
            when (expiry < addUTCTime timeout now) $
                setCookie key domain uuid expiry


------------------------------------------------------------------------------
reset :: Session a -> Session b
reset (Session now muuid mtimeout _) = Session now muuid mtimeout Nothing


------------------------------------------------------------------------------
renew :: NominalDiffTime -> Session a -> Session a
renew timeout (Session now muuid _ mpayload) =
    Session now muuid (Just timeout) mpayload


------------------------------------------------------------------------------
get :: Session a -> Maybe a
get = _payload


------------------------------------------------------------------------------
put :: a -> Session a -> Session a
put payload (Session now muuid mtimeout _) =
    Session now muuid mtimeout (Just payload)


------------------------------------------------------------------------------
modify :: (a -> a) -> Session a -> Session a
modify = fmap


------------------------------------------------------------------------------
readCookie :: Cookie -> Maybe UUID
readCookie = UUID.fromASCIIBytes . cookieValue


------------------------------------------------------------------------------
viewCookie :: ByteString -> Snap (Maybe UUID)
viewCookie key = runMaybeT $ do
    cookie <- MaybeT $ getCookie key
    case readCookie cookie of
        Nothing -> lift (expireCookie cookie) >> empty
        Just uuid -> pure uuid


------------------------------------------------------------------------------
setCookie :: ByteString -> Maybe ByteString -> UUID -> UTCTime -> Snap ()
setCookie key domain uuid time =
    modifyResponse $ addResponseCookie cookie
  where
    cookie = Cookie key value (Just time) domain (Just "/") True True
    value = UUID.toASCIIBytes uuid


------------------------------------------------------------------------------
unsetCookie :: ByteString -> Maybe ByteString -> Snap ()
unsetCookie key domain = do
    (cookies, without) <- partition ((== key) . cookieName)
        <$> getsRequest rqCookies
    modifyRequest $ \rq -> rq {rqCookies = without}
    unless (null cookies) $ expireCookie cookie
  where
    cookie = Cookie key mempty Nothing domain (Just "/") False False
