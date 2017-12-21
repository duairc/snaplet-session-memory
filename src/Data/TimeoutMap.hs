{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Data.TimeoutMap
    ( TimeoutMap
    , lookup
    , insert, renew
    , adjust, update
    , delete
    , clean
    )
where

-- base ----------------------------------------------------------------------
import           Data.Bifunctor (bimap, first, second)
import           Data.Maybe (fromJust)
import           Data.Semigroup (Semigroup)
import           Prelude hiding (lookup)


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable)


-- time ----------------------------------------------------------------------
import           Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)


-- unordered-containers ------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H


------------------------------------------------------------------------------
newtype TimeoutMap k a = TimeoutMap (HashMap k (a, UTCTime))
  deriving (Semigroup, Monoid)


------------------------------------------------------------------------------
lookup :: (Eq k, Hashable k)
    => k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe a)
lookup = fmap fst ... mutateWith Nothing (\a -> (Just (a, Nothing), a))


------------------------------------------------------------------------------
insert :: (Eq k, Hashable k)
    => a -> NominalDiffTime
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, UTCTime)
insert a t = fmap fromJust ... mutate (Just (a, t)) (\_ -> Just (a, Just t))


------------------------------------------------------------------------------
renew :: (Eq k, Hashable k)
    => NominalDiffTime
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe UTCTime)
renew = adjust . flip (,) . Just


------------------------------------------------------------------------------
adjust :: (Eq k, Hashable k)
    => (a -> (a, Maybe NominalDiffTime))
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe UTCTime)
adjust = update . (Just .)


------------------------------------------------------------------------------
update :: (Eq k, Hashable k)
    => (a -> Maybe (a, Maybe NominalDiffTime))
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe UTCTime)
update = mutate Nothing


------------------------------------------------------------------------------
delete :: (Eq k, Hashable k)
    => k -> UTCTime -> TimeoutMap k a -> TimeoutMap k a
delete = fst ... mutate Nothing (const Nothing)


------------------------------------------------------------------------------
mutate :: (Eq k, Hashable k)
    => Maybe (a, NominalDiffTime)
    -> (a -> Maybe (a, Maybe NominalDiffTime))
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe UTCTime)
mutate inserter updater = fmap snd ... mutateWith inserter ((, ()) . updater)


------------------------------------------------------------------------------
mutateWith :: (Eq k, Hashable k)
    => Maybe (a, NominalDiffTime)
    -> (a -> (Maybe (a, Maybe NominalDiffTime), b))
    -> k -> UTCTime -> TimeoutMap k a
    -> (TimeoutMap k a, (Maybe b, Maybe UTCTime))
mutateWith inserter updater key now = tmap (alterWith (expiry . go) key)
  where
    go (Just (a, then_)) | then_ >= now = bimap time Just (updater a)
      where
        time = fmap (second (maybe then_ (max then_ . flip addUTCTime now)))
    go _ = (second (flip addUTCTime now) <$> inserter, Nothing)
    expiry (mutation, result) = (mutation, (result, snd <$> mutation))


------------------------------------------------------------------------------
clean :: UTCTime -> TimeoutMap k a -> TimeoutMap k a
clean = tmap_ . H.filter . (. snd) . (<=)


------------------------------------------------------------------------------
tmap :: (HashMap k (a, UTCTime) -> (HashMap k (a, UTCTime), b))
    -> TimeoutMap k a -> (TimeoutMap k a, b)
tmap f (TimeoutMap m) = first TimeoutMap $ f m


------------------------------------------------------------------------------
tmap_ :: (HashMap k (a, UTCTime) -> HashMap k (a, UTCTime))
    -> TimeoutMap k a -> TimeoutMap k a
tmap_ f (TimeoutMap m) = TimeoutMap $ f m


------------------------------------------------------------------------------
alterWith :: (Eq k, Hashable k)
    => (Maybe a -> (Maybe a, b)) -> k -> HashMap k a -> (HashMap k a, b)
alterWith f k m = first (maybe delete_ insert_) $ f lookup_
  where
    lookup_ = H.lookup k m
    delete_ = H.delete k m
    insert_ v = H.insert k v m
{-# INLINE alterWith #-}


------------------------------------------------------------------------------
(...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(...) f g a b c = f (g a b c)
infixr 9 ...
