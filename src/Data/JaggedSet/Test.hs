{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, OverloadedStrings #-}
module Data.JaggedSet.Test where

-- First, some imports:

import Data.JaggedSet

-- for our underlying type:
import qualified Data.Text as T
import Data.Text (Text(..))
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Lens            -- Lenses will make it much simpler to write some
import Data.Lens.Template   -- of the required instances for JaggedSet

import Data.Acid                      -- All of these are needed for acid-state
import Data.Acid.Advanced
import Data.Data (Data, Typeable)
import Data.SafeCopy
import Control.Monad.State (MonadIO)
import Control.Monad.Reader (ask)

import Data.Aeson           -- So we can define JSON representations

-- First, we create one big ADT for the data that we're going to store.

data SdbRecord = Album
                    { _id :: Int
                    , _title :: Text
                    , _sortTitle :: Text
                    , _artist :: Text
                    , _sortArtist :: Text
                    , _art :: [Art]
                    , _thumbnail :: [Art]
                    , _year :: Int
                    , _date :: Integer
                    } |
                 Song
                    { _songid :: Int
                    , _songTitle :: Text
                    , _songTrack :: Int
                    , _songAlbumTitle :: Text
                    , _songAlbumSortTitle :: Text
                    , _songArtist :: Text
                    , _songSortArtist :: Text
                    }
            deriving (Show, Eq, Ord, Data, Typeable)

data Art = Art
          { _mime :: B.ByteString
          , _file :: String
          , _updated :: Integer
          }
          deriving (Show, Eq, Ord, Data, Typeable)

-- Next, an associated index type.
data StereoidKey = AlbumTitle Text
                 | AlbumSortTitle Text
                 | AlbumArtist Text
                 | AlbumId Int
                 | SongId Int
                 | AlbumSortArtist Text deriving (Typeable)

-- The index type is required to have instances of numerous type classes.
instance Bounded StereoidKey where
    -- s/b done with Template Haskell eventually
    minBound    = AlbumTitle undefined
    maxBound    = AlbumSortArtist undefined

instance Enum   StereoidKey where
    -- s/b done with Template Haskell eventually
    fromEnum (AlbumTitle _)         = 0
    fromEnum (AlbumSortTitle _)     = 1
    fromEnum (AlbumArtist _)        = 2
    fromEnum (AlbumId _)            = 3
    fromEnum (SongId _)             = 4
    fromEnum (AlbumSortArtist _)    = 5
    toEnum 0 = (AlbumTitle undefined)
    toEnum 1 = (AlbumSortTitle undefined)
    toEnum 2 = (AlbumArtist undefined)
    toEnum 3 = (AlbumId undefined)
    toEnum 4 = (SongId undefined)
    toEnum 5 = (AlbumSortArtist undefined)

-- The IndexKey class is where things start to get interesting. 'toKey' converts
-- a value of our index type to a Key based on how we want to index it.
instance IndexKey StereoidKey where
    -- do we leave this partial or add all of the tedious boilerplate?
    toKey (AlbumTitle x)      = BSKey $ E.encodeUtf8 x
    toKey (AlbumSortTitle x)  = BSKey $ E.encodeUtf8 x
    toKey (AlbumArtist x)     = BSKey $ E.encodeUtf8 x
    toKey (AlbumId x)         = PrimaryKey x
    toKey (SongId x)          = PrimaryKey x
    toKey (AlbumSortArtist x) = BSKey $ E.encodeUtf8 x
    fromKey (AlbumId _) (PrimaryKey x) = AlbumId x
    fromKey (SongId _) (PrimaryKey x)  = SongId x


testli :: JaggedSet SdbRecord StereoidKey
testli = fromList [ Album 1 "Somewhere in Time" "Somewhere in Time" "Iron Maiden" "Iron Maiden" [] [] 1986 123456
                  , Album 2 "Powerslave" "Powerslave" "Iron Maiden" "Iron Maiden" [] [] 1984 123457
                  , Album 3 "Seventh Son of a Seventh Son" "Seventh Son of a Seventh Son" "Iron Maiden" "Iron Maiden" [] [] 1988 123458
                  , Song 4 "Caught Somewhere in Time" 1 "Somewhere in Time" "Somewhere in Time" "Iron Maiden" "Iron Maiden"
                  , Song 5 "Wasted Years" 2 "Somewhere in Time" "Somewhere in Time" "Iron Maiden" "Iron Maiden"
                  , Song 4 "Sea of Madness" 3 "Somewhere in Time" "Somewhere in Time" "Iron Maiden" "Iron Maiden"
                  , Album 7 "Giant Pin" "Giant Pin" "The Nels Cline Singers" "Nels Cline Singers" [] [] 2002 124444
                  ]

data Sdb = Sdb { _sdb :: JaggedSet SdbRecord StereoidKey } deriving (Typeable)

$(makeLens ''SdbRecord)
$(makeLens ''Art)
$(makeLens ''Sdb)

instance Indexable SdbRecord where
    type IndexOf SdbRecord = StereoidKey
    project (AlbumTitle _)      x@(Album {}) = Just [AlbumTitle (title ^$ x)]
    project (AlbumSortTitle _)  x@(Album {}) = Just [AlbumSortTitle (sortTitle ^$ x)]
    project (AlbumArtist _)     x@(Album {}) = Just [AlbumArtist (artist ^$ x)]
    project (AlbumId _)         x@(Album {}) = Just [AlbumId (id ^$ x)]
    project (SongId _)          x@(Album {}) = Nothing
    project (AlbumSortArtist _) x@(Album {}) = Just [AlbumSortArtist (sortArtist ^$ x)]

    project (AlbumTitle _)      x@(Song {}) = Just [AlbumTitle (songAlbumTitle ^$ x)]
    project (AlbumSortTitle _)  x@(Song {}) = Just [AlbumSortTitle (songAlbumSortTitle ^$ x)]
    project (AlbumArtist _)     x@(Song {}) = Just [AlbumArtist (songArtist ^$ x)]
    project (AlbumId _)         x@(Song {}) = Nothing
    project (SongId _)          x@(Song {}) = Just [SongId (songid ^$ x)]
    project (AlbumSortArtist _) x@(Song {}) = Just [AlbumSortArtist (songSortArtist ^$ x)]

    reflect (AlbumTitle y) x@(Song {}) = Just (songAlbumTitle ^= y $ x)
    reflect (AlbumSortTitle y) x@(Song {}) = Just (songAlbumSortTitle ^= y $ x)
    reflect (AlbumArtist y) x@(Song {}) = Just (songArtist ^= y $ x)
    reflect (AlbumId y) x@(Song {}) = Just x
    reflect (SongId y) x@(Song {}) = Just (songid ^= y $ x)
    reflect (AlbumSortArtist y) x@(Song {}) = Just (songSortArtist ^= y $ x)

    reflect (AlbumTitle y) x@(Album {}) = Just (title ^= y $ x)
    reflect (AlbumSortTitle y) x@(Album {}) = Just (sortTitle ^= y $ x)
    reflect (AlbumArtist y) x@(Album {}) = Just (artist ^= y $ x)
    reflect (AlbumId y) x@(Album {}) = Just (id ^= y $ x)
    reflect (SongId y) x@(Album {}) = Just x
    reflect (AlbumSortArtist y) x@(Album {}) = Just (sortArtist ^= y $ x)

$(deriveSafeCopy 0 'base ''Art)
$(deriveSafeCopy 0 'base ''SdbRecord)
$(deriveSafeCopy 0 'base ''Sdb)

testDb :: JaggedSet SdbRecord StereoidKey
testDb = empty

albums :: StereoidKey
albums = AlbumId undefined

songs :: StereoidKey
songs = SongId undefined

testQ :: StereoidKey -> [SdbRecord]
testQ k = queryList (only albums >/< like k) testDb

getSdb :: Query Sdb (JaggedSet SdbRecord StereoidKey)
getSdb = do db <- ask
            return $ _sdb db

$(makeAcidic ''Sdb ['getSdb])

runQuery :: (Monad m, MonadIO m) => AcidState Sdb -> JaggedQuery SdbRecord StereoidKey (Selection StereoidKey) -> m [SdbRecord]
runQuery acid q = do db <- query' acid (GetSdb)
                     return $ queryList q db



