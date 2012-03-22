{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
module Data.JaggedSet.Test where

import Data.Lens
import Data.Lens.Template
import Data.JaggedSet
import qualified Data.Text as T
import Data.Text (Text(..))
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.UTF8 as B
import Data.Data (Data, Typeable)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Acid
import Data.Acid.Advanced
import qualified Data.Vector as V
import Data.SafeCopy

data StereoidKey = AlbumTitle Text
                 | AlbumSortTitle Text
                 | AlbumArtist Text
                 | AlbumId Int
                 | SongId Int
                 | AlbumSortArtist Text

instance Bounded StereoidKey where
    minBound    = AlbumTitle undefined
    maxBound    = AlbumSortArtist undefined

instance Enum   StereoidKey where
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


instance IndexKey StereoidKey where
    toKey (AlbumTitle x)      = BSKey $ E.encodeUtf8 x
    toKey (AlbumSortTitle x)  = BSKey $ E.encodeUtf8 x
    toKey (AlbumArtist x)     = BSKey $ E.encodeUtf8 x
    toKey (AlbumId x)         = PrimaryKey x
    toKey (SongId x)          = PrimaryKey x
    toKey (AlbumSortArtist x) = BSKey $ E.encodeUtf8 x

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
                    , _songAlbumTitle :: Text
                    , _songAlbumSortTitle :: Text
                    , _songArtist :: Text
                    , _songSortArtist :: Text
                    }
            deriving (Eq, Ord, Data, Typeable)


data Art = Art
          { _mime :: B.ByteString
          , _file :: String
          , _updated :: Integer
          }
          deriving (Eq, Ord, Data, Typeable)

data Sdb = Sdb (JaggedSet SdbRecord StereoidKey) deriving (Typeable)

$(makeLens ''SdbRecord)
$(makeLens ''Art)

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
testQ k = queryList (only albums >/< submap k) testDb




