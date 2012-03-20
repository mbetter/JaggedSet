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

data StereoidKey = AlbumTitle Text
                 | AlbumSortTitle Text
                 | AlbumArtist Text
                 | AlbumSortArtist Text

instance Bounded StereoidKey where
    minBound    = AlbumTitle undefined
    maxBound    = AlbumSortArtist undefined

instance Enum   StereoidKey where
    fromEnum (AlbumTitle _)         = 0
    fromEnum (AlbumSortTitle _)     = 1
    fromEnum (AlbumArtist _)        = 2
    fromEnum (AlbumSortArtist _)    = 3
    toEnum 0 = (AlbumTitle undefined)
    toEnum 1 = (AlbumSortTitle undefined)
    toEnum 2 = (AlbumArtist undefined)
    toEnum 3 = (AlbumSortArtist undefined)


instance IndexKey StereoidKey where
    toKey (AlbumTitle x)      = BSKey $ E.encodeUtf8 x
    toKey (AlbumSortTitle x)  = BSKey $ E.encodeUtf8 x
    toKey (AlbumArtist x)     = BSKey $ E.encodeUtf8 x
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

$(makeLens ''SdbRecord)
$(makeLens ''Art)

instance Indexable SdbRecord where
    type IndexOf SdbRecord = StereoidKey
    project (AlbumTitle _)      x@(Album {}) = Just [AlbumTitle (title ^$ x)]
    project (AlbumSortTitle _)  x@(Album {}) = Just [AlbumSortTitle (sortTitle ^$ x)]
    project (AlbumArtist _)     x@(Album {}) = Just [AlbumArtist (artist ^$ x)]
    project (AlbumSortArtist _) x@(Album {}) = Just [AlbumSortArtist (sortArtist ^$ x)]

    project (AlbumTitle _)      x@(Song {}) = Just [AlbumTitle (songAlbumTitle ^$ x)]
    project (AlbumSortTitle _)  x@(Song {}) = Just [AlbumSortTitle (songAlbumSortTitle ^$ x)]
    project (AlbumArtist _)     x@(Song {}) = Just [AlbumArtist (songArtist ^$ x)]
    project (AlbumSortArtist _) x@(Song {}) = Just [AlbumSortArtist (songSortArtist ^$ x)]

