{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, OverloadedStrings, TypeFamilies, FlexibleInstances #-}

module Data.JaggedSet where

import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.ByteString.UTF8 as B
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Trie as BT 
import qualified Data.Trie.Convenience as BT
import Data.List (foldl')
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Prelude hiding (lookup)



class Indexable a where
    type IndexOf a
    project     :: IndexOf a -> a -> Maybe [IndexOf a]

class (Bounded a, Enum a) => IndexKey a where
    toKey    :: a -> Key

data Index  = BSTrieIndex { unTrieIndex :: !(BT.Trie IS.IntSet) }
            | PrimaryIndex { unPrimaryIndex :: !(IS.IntSet) }
            | IntMapIndex { unIntMapIndex :: !(IM.IntMap IS.IntSet) }
            
data Key = IntKey { unIntKey :: Int } 
         | BSKey { unBSKey :: B.ByteString }
         | PrimaryKey {unPrimaryKey :: Int }

data JaggedSet a i = JaggedSet
                      { elements      :: !(IM.IntMap a)
                      , maxKey        :: !Int
                      , indicies      :: !(V.Vector Index)
                      }

type JaggedQuery e i a = Reader (JaggedSet e i) a

data Margin a = Open a
              | Closed a
              | Infinite a

unMargin (Open a) = a
unMargin (Closed a) = a
unMargin (Infinite a) = a

data Order = Ascending
           | Descending

data Selection k = Nothing'
                 | Set !IS.IntSet
                 | Everything
                 | Range (Margin k) (Margin k)
                 | Submap k
                 | Union (Selection k) (Selection k)
                 | Intersection (Selection k) (Selection k)

resolve :: forall a i.(Indexable a, IndexKey i, IndexOf a ~ i) => Selection i
                                                               -> JaggedQuery a i (Selection i)
resolve Nothing' = return Nothing'
resolve (Set a) = if IS.null a
                    then return $ Nothing'
                    else return $ Set a
resolve Everything = return Everything
resolve (Union a b)
  = do a <- resolve a
       case a of
         Nothing' -> resolve b
         Set is -> do b <- resolve b
                      return $ case b of
                                 Nothing' -> Set is
                                 Set js -> Set $ IS.union is js
                                 Everything -> Everything
         Everything -> return Everything

resolve (Range a b)
  = do ind <- index (unMargin a) <$> ask
       case ind of
        IntMapIndex ind1 -> do let ind2 = case a of
                                            Infinite k -> ind1
                                            Open k     -> snd $ IM.split (unIntKey $ toKey k) ind1
                                            Closed k   -> let (_, i, af) = IM.splitLookup (unIntKey $ toKey $ unMargin a) ind1
                                                          in maybe af (\x -> IM.insert (unIntKey $ toKey k) x af) i
                               let ind3 = case b of
                                            Infinite k -> ind2
                                            Open k     -> fst $ IM.split (unIntKey $ toKey k) ind2
                                            Closed k   -> let (be, i, _) = IM.splitLookup (unIntKey $ toKey $ unMargin b) ind2
                                                          in maybe be (\x -> IM.insert (unIntKey $ toKey k) x be) i 
                               return $ case IM.elems ind3 of
                                          [] -> Nothing'
                                          is -> Set $ foldl' IS.union IS.empty is
        BSTrieIndex ind1 -> do let ind2 = case a of
                                            Infinite k -> ind1
                                            Open k     -> BT.fromList $ dropWhile (\(x,_) -> (x <= (unBSKey $ toKey $ unMargin a))) (BT.toList ind1)
                                            Closed k   -> BT.fromList $ dropWhile (\(x,_) -> (x < (unBSKey $ toKey $ unMargin a))) (BT.toList ind1)
                               let ind3 = case b of
                                            Infinite k -> ind2
                                            Open k     -> BT.fromList $ takeWhile (\(x,_) -> (x < (unBSKey $ toKey $ unMargin b))) (BT.toList ind2)
                                            Closed k   -> BT.fromList $ takeWhile (\(x,_) -> (x <= (unBSKey $ toKey $ unMargin b))) (BT.toList ind2)
                               return $ case BT.elems ind3 of
                                          [] -> Nothing'
                                          is -> Set $ foldl' IS.union IS.empty is
        PrimaryIndex ind1 -> do let ind2 = case a of
                                            Infinite k -> ind1
                                            Open k     -> snd $ IS.split (unPrimaryKey $ toKey k) ind1
                                            Closed k   -> let (_, i, af) = IS.splitMember (unPrimaryKey $ toKey $ unMargin a) ind1
                                                          in if i
                                                               then IS.insert (unPrimaryKey $ toKey k) af
                                                               else af
                                let ind3 = case b of
                                            Infinite k -> ind2
                                            Open k     -> fst $ IS.split (unPrimaryKey $ toKey k) ind2
                                            Closed k   -> let (be, i, _) = IS.splitMember (unPrimaryKey $ toKey $ unMargin b) ind2
                                                          in if i
                                                               then IS.insert (unPrimaryKey $ toKey k) be
                                                               else be
                                return $ if IS.null ind3 
                                           then Nothing'
                                           else  Set ind3
resolve (Submap a)
  = do ind <- index a <$> ask
       case ind of
        BSTrieIndex ind1 -> let ind2 = BT.submap (unBSKey $ toKey a) ind1
                            in return $ if BT.null ind2
                                          then Nothing'
                                          else Set $ IS.unions $ BT.elems ind2
        IntMapIndex ind1 -> resolve (Range (Closed a) (Infinite undefined)) -- Can't think of an operation isomorphic to 'submap'
        PrimaryIndex ind1 -> resolve (Range (Closed a) (Infinite undefined))
resolve (Intersection a b)
  = do a <- resolve a
       case a of
         Nothing' -> return a
         Set is -> do b <- resolve b
                      return $ case b of
                                 Nothing' -> Nothing'
                                 Set js -> Set $ IS.intersection is js
                                 Everything -> Set is
         Everything -> resolve b

nothing :: JaggedQuery a i (Selection i)
nothing = return Nothing'

everything :: JaggedQuery a i (Selection i)
everything = return Everything

equals :: (Indexable a, IndexKey i, IndexOf a ~ i) => i -> JaggedQuery a i (Selection i)
equals i = return $ Range (Closed i) (Closed i)

greater :: (Indexable a, IndexKey i, IndexOf a ~ i) => i -> JaggedQuery a i (Selection i)
greater i = return $ Range (Open i) (Infinite i)

greaterEq :: (Indexable a, IndexKey i, IndexOf a ~ i) => i -> JaggedQuery a i (Selection i)
greaterEq i = return $ Range (Closed i) (Infinite i)

lower :: (Indexable a, IndexKey i, IndexOf a ~ i) => i -> JaggedQuery a i (Selection i)
lower i = return $ Range (Infinite i) (Open i)

lowerEq :: (Indexable a, IndexKey i, IndexOf a ~ i) => i -> JaggedQuery a i (Selection i)
lowerEq i = return $ Range (Infinite i) (Closed i)

range :: (Indexable a, IndexKey i, IndexOf a ~ i) => Margin i 
                                               -> Margin i 
                                               -> JaggedQuery a i (Selection i)
range a b = return $ Range a b

submap :: (Indexable a, IndexKey i, IndexOf a ~ i) => i -> JaggedQuery a i (Selection i)
submap = return . Submap

union :: JaggedQuery a i (Selection i) -> JaggedQuery a i (Selection i) -> JaggedQuery a i (Selection i)
union a b = a >>= \a'-> b >>= \b'-> (return $ Union a' b')

intersection :: JaggedQuery a i (Selection i) -> JaggedQuery a i (Selection i) -> JaggedQuery a i (Selection i)
intersection a b = a >>= \a'-> b >>= \b'-> (return $ Intersection a' b')


querySize :: (Indexable a, IndexKey i, IndexOf a ~ i) => JaggedQuery a i (Selection i) -> JaggedSet a i -> Int
querySize q s = case runReader (q >>= resolve) s of
                     Nothing' -> 0
                     Set is -> IS.size is
                     Everything -> IM.size (elements s)

querySet :: (Indexable a, Ord a, IndexKey i, IndexOf a ~ i) => JaggedQuery a i (Selection i) -> JaggedSet a i -> S.Set a
querySet q s = S.fromList $ queryList q s 

queryList :: (Indexable a, IndexKey i, IndexOf a ~ i) => JaggedQuery a i (Selection i) -> JaggedSet a i -> [a]
queryList q s = case runReader (q >>= resolve) s of
                     Nothing' -> []
                     Set is -> map
                                        (\i-> fromMaybe (error "corrupted JaggedSet (error 2b)") $ IM.lookup i (elements s))
                                        (IS.toList is)
                     Everything -> toList s

lookup :: (Indexable a, IndexKey i, IndexOf a ~ i) => i -> JaggedSet a i -> Maybe a
lookup i s = case queryList (equals i) s of
                        [] -> Nothing
                        (x:_) -> Just x

updateLookup :: (Indexable a, IndexKey i, IndexOf a ~ i) => (a -> Maybe a) -> i -> JaggedSet a i -> (Maybe a, JaggedSet a i)
updateLookup = updateLookupUnsafe [minBound..maxBound]

updateLookupUnsafe :: (Indexable a, IndexKey i, IndexOf a ~ i) => [i] -> (a -> Maybe a) -> i -> JaggedSet a i -> (Maybe a, JaggedSet a i)
updateLookupUnsafe ix f i s
                    = case lookup i s of
                        Nothing -> (Nothing, s)
                        Just a -> (f a, updateUnsafe ix f (equals i) s)

empty :: forall a i.(IndexKey i) => JaggedSet a i
empty = JaggedSet
          { elements = IM.empty
          , maxKey = minBound
          , indicies = V.fromList (map 
                                    (\x -> case toKey x of
                                            IntKey k     -> IntMapIndex IM.empty
                                            BSKey k      -> BSTrieIndex BT.empty
                                            PrimaryKey k -> PrimaryIndex IS.empty )
                                     ([minBound..maxBound] :: [i])
                                    )
          }
        
size :: JaggedSet a i -> Int
size = IM.size . elements

insert :: forall a i.(Indexable a, IndexKey i, IndexOf a ~ i) => a -> JaggedSet a i -> JaggedSet a i
insert x s = s { elements = IM.insert key x (elements s)
               , maxKey   = key
               , indicies = V.accum
                              (foldl'
                                (\x y-> case x of
                                         BSTrieIndex m -> BSTrieIndex $ BT.insertWith
                                                                        IS.union
                                                                        (unBSKey $ toKey y)
                                                                        (IS.singleton key)
                                                                        m
                                         IntMapIndex m -> IntMapIndex $ IM.insertWith
                                                                        IS.union
                                                                        (unIntKey $ toKey y)
                                                                        (IS.singleton key)
                                                                        m
                                         PrimaryIndex m -> PrimaryIndex $ IS.insert
                                                                          key
                                                                          m
                                )
                              )

                              (indicies s)
                              
                              (mapMaybe (\c ->
                                (fmap . (,)) (fromEnum c) (project c x))
                                [minBound..maxBound :: i]
                              )
               }
             where
                key = succ $ maxKey s

delete :: (Indexable a, IndexKey i, IndexOf a ~ i) => JaggedQuery a i (Selection i) -> JaggedSet a i -> JaggedSet a i
delete = update (const Nothing)
                       
update :: (Indexable a, IndexKey i, IndexOf a ~ i) => (a -> Maybe a) -> JaggedQuery a i (Selection i) -> JaggedSet a i -> JaggedSet a i
update = updateUnsafe [minBound..maxBound]

updateUnsafe :: (Indexable a, IndexKey i, IndexOf a ~ i) => [i] -> (a -> Maybe a) -> JaggedQuery a i (Selection i) -> JaggedSet a i -> JaggedSet a i
updateUnsafe ix f query
  = \s-> case runReader (query >>= resolve) s of
           Nothing' -> s
           Set xs -> foldl' g s (IS.toList xs)
           Everything -> fromList $ mapMaybe f $ toList s
  where -- acc is the working JaggedSet, i is the current primary key of the query result
    g acc i = case IM.lookup i (elements acc) of
                Nothing -> acc -- if i isn't in the map, bail.
                Just a -> -- a is the current item
                  case f a of
                    Nothing -> -- this means we want to delete the element.
                      acc { elements = IM.delete i (elements acc)
                          , indicies = V.accum
                                         (foldl'
                                           (\m k-> case m of
                                                     IntMapIndex m' -> IntMapIndex $ IM.update
                                                                                         (\is-> let is' = IS.delete i is
                                                                                                in if IS.null is'
                                                                                                      then Nothing
                                                                                                      else Just is'
                                                                                         )
                                                                                         (unIntKey $ toKey k)
                                                                                         m'
                                                     PrimaryIndex m' -> PrimaryIndex $ IS.delete i m'
                                                     BSTrieIndex m' -> BSTrieIndex $ BT.update
                                                                                         (\is-> let is' = IS.delete i is
                                                                                                in if IS.null is'
                                                                                                      then Nothing
                                                                                                      else Just is'
                                                                                         )
                                                                                         (unBSKey $ toKey k)
                                                                                         m'
                                           )
                                         )

                                         (indicies acc)

                                         (mapMaybe (\c ->
                                           (fmap . (,)) (fromEnum c) (project c a))
                                           ix
                                         )
                          }
                    Just a' ->
                      acc { elements = IM.insert i a' (elements acc)
                          , indicies = V.accum
                                         (foldl'
                                           (\m k-> case m of
                                                     IntMapIndex m' -> IntMapIndex $ IM.insertWith
                                                                                    IS.union
                                                                                    (unIntKey $ toKey k)
                                                                                    (IS.singleton i)
                                                                                    m'
                                                     PrimaryIndex m' -> PrimaryIndex $ IS.insert i m'
                                                     BSTrieIndex m' -> BSTrieIndex $ BT.insertWith
                                                                                     IS.union
                                                                                     (unBSKey $ toKey k)
                                                                                     (IS.singleton i)
                                                                                     m'
                                           )
                                         )
                                         (V.accum
                                           (foldl'
                                             (\m k-> case m of
                                                       IntMapIndex m' -> IntMapIndex $ IM.update
                                                                                         (\is-> let is' = IS.delete i is
                                                                                                in if IS.null is'
                                                                                                      then Nothing
                                                                                                      else Just is'
                                                                                         )
                                                                                         (unIntKey $ toKey k)
                                                                                         m'
                                                       PrimaryIndex m' -> PrimaryIndex $ IS.delete i m'
                                                       BSTrieIndex m' -> BSTrieIndex $ BT.update
                                                                                           (\is-> let is' = IS.delete i is
                                                                                                  in if IS.null is'
                                                                                                        then Nothing
                                                                                                        else Just is'
                                                                                           )
                                                                                           (unBSKey $ toKey k)
                                                                                           m'
                                             )
                                           )
                                           (indicies acc)

                                           (mapMaybe (\c ->
                                             (fmap . (,)) (fromEnum c) (project c a))
                                             ix
                                           )

                                         )
                                         (mapMaybe (\c ->
                                           (fmap . (,)) (fromEnum c) (project c a'))
                                           ix
                                         )
                          }

toList :: JaggedSet a i -> [a]
toList = IM.elems . elements

fromList :: (Indexable a, IndexKey i, IndexOf a ~ i) => [a] -> JaggedSet a i
fromList = foldl' (flip insert) empty

index :: (IndexKey i) => i -> JaggedSet a i -> Index
index i s = (indicies s) V.! (fromEnum i)

{-
getByIds :: IM.IntMap a -> IS.IntSet -> IM.IntMap a
getByIds im xs = IM.intersection im (fromSet xs)
                 where  
                    fromSet s = IM.fromAscList $ zip (IS.toAscList s) (repeat ())

ix :: forall a i.(Indexable a, IndexKey i, IndexOf a ~ i) => JaggedSet a i -> i -> IS.IntSet
ix s k = case toKey k of
             BSKey _  -> IS.unions $ BT.elems $ unTrieIndex $ index k s
             IntKey _  -> IS.unions $ IM.elems $ unIntMapIndex $ index k s
             PrimaryKey _ -> unPrimaryIndex $ index k s

(?=) :: forall a i.(Indexable a, IndexKey i, IndexOf a ~ i) => i -> JaggedSet a i -> IM.IntMap a
(?=) k d = case toKey k of
            BSKey _  -> maybe IM.empty
                            (\x -> getByIds (elements d) x)
                            (BT.lookup (unBSKey $ toKey k) (unTrieIndex $ index k d) )
            IntKey _ -> maybe IM.empty
                            (\x -> getByIds (elements d) x)
                            (IM.lookup (unIntKey $ toKey k) (unIntMapIndex $ index k d) )
            PrimaryKey _ -> if IS.member (i k) (unPrimaryIndex $ index k d)
                                then maybe IM.empty
                                      (\x -> IM.singleton (i k) x)
                                      (IM.lookup (i k) (elements d))
                                else IM.empty
                            where i = (unIntKey . toKey)
 
(%=) :: forall a i.(Indexable a, IndexKey i, IndexOf a ~ i) => i -> JaggedSet a i -> IM.IntMap a
(%=) k d = case toKey k of
            BSKey _ -> getByIds (elements d) (ids k d)  
                       where
                        ids x s = IS.unions $ BT.elems $ BT.submap (unBSKey $ toKey x) (unTrieIndex $ index x s) 
-}
