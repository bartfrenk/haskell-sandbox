{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}

module HaxlTutorial.Scratch where

import Data.Ord
import qualified Data.Map as Map
import Data.List (sortBy)
import GHC.Generics


data PostId -- identifies a post

data Date = Date deriving (Eq, Ord) -- a calendar date

data PostContent -- the content of a post

data PostInfo = PostInfo
  { postId :: PostId
  , postDate :: Date
  , postTopic :: String
  }

data Fetch a = Done a | Blocked (Fetch a)
  deriving Generic

instance Functor Fetch where
  fmap f (Done x) = Done (f x)
  fmap f (Blocked c) = Blocked (fmap f c)

instance Monad Fetch where
  return = Done
  Done a >>= k = k a
  Blocked c >>= k = Blocked (c >>= k)


instance Applicative Fetch where
  pure = return

getPostIds :: Fetch [PostId]
getPostIds = undefined

getPostInfo :: PostId -> Fetch PostInfo
getPostInfo = undefined

getPostContent :: PostId -> Fetch PostContent
getPostContent = undefined

getPostViews :: PostId -> Fetch Int
getPostViews = undefined

data Html

renderPosts :: [(PostInfo, PostContent)] -> Html
renderPosts = undefined

renderPage :: Html -> Html -> Html
renderPage = undefined

blog :: Fetch Html
blog = renderPage <$> leftPane <*> mainPane

leftPane :: Fetch Html
leftPane = renderSidePane <$> popularPosts <*> topics

renderSidePane :: Html -> Html -> Html
renderSidePane = undefined

mainPane :: Fetch Html
mainPane = do
  posts <- getAllPostsInfo
  let ordered =
        take 5 $
        sortBy (flip (comparing postDate)) posts
  content <- mapM (getPostContent . postId) ordered
  return $ renderPosts (zip ordered content)

getAllPostsInfo :: Fetch [PostInfo]
getAllPostsInfo = mapM getPostInfo =<< getPostIds

getPostDetails :: PostId -> Fetch (PostInfo, PostContent)
getPostDetails pid = (,) <$> getPostInfo pid <*> getPostContent pid

popularPosts :: Fetch Html
popularPosts = do
  pids <- getPostIds
  views <- mapM getPostViews pids
  let ordered =
        take 5 $ map fst $
        sortBy (flip (comparing snd))
               (zip pids views)
  content <- mapM getPostDetails ordered
  return $ renderPostList content

renderPostList :: [(PostInfo, PostContent)] -> Html
renderPostList = undefined

topics :: Fetch Html
topics = do
  posts <- getAllPostsInfo
  let topiccounts =
        Map.fromListWith (+)
        [(postTopic p, 1 :: Integer) | p <- posts]
  return $ renderTopics topiccounts

renderTopics :: Map.Map String Integer -> Html
renderTopics = undefined
