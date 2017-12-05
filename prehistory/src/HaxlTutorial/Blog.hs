module HaxlTutorial.Blog where

import Data.Ord
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sortBy)

import HaxlTutorial.Fetch

blog :: Fetch Html
blog = renderPage <$> leftPane <*> mainPane

leftPane :: Fetch Html
leftPane =
  let renderedPopularPosts = renderPostList <$> popularPosts
      renderedTopics = renderTopics <$> topics
  in renderSidePane <$> renderedPopularPosts <*> renderedTopics

mainPane :: Fetch Html
mainPane = do
  posts <- getAllPostsInfo
  let ordered =
        take 5 $
        sortBy (flip (comparing postDate)) posts
  content <- mapM (getPostContent . postId) ordered
  return $ renderPosts (zip ordered content)

data PostId -- identifies a post

data Date = Date deriving (Eq, Ord) -- a calendar date

data PostContent -- the content of a post

type Topic = String

data PostInfo = PostInfo
  { postId :: PostId
  , postDate :: Date
  , postTopic :: Topic
  }

-- | Stubbed atomic fetching

getPostIds :: Fetch [PostId]
getPostIds = undefined

getPostInfo :: PostId -> Fetch PostInfo
getPostInfo = undefined

getPostContent :: PostId -> Fetch PostContent
getPostContent = undefined

getPostViews :: PostId -> Fetch Int
getPostViews = undefined

-- | Compound fetching

getAllPostsInfo :: Fetch [PostInfo]
getAllPostsInfo = mapM getPostInfo =<< getPostIds

getPostDetails :: PostId -> Fetch (PostInfo, PostContent)
getPostDetails pid = (,) <$> getPostInfo pid <*> getPostContent pid

popularPosts :: Fetch [(PostInfo, PostContent)]
popularPosts = do
  pids <- getPostIds
  views <- mapM getPostViews pids
  let ordered =
        take 5 $ map fst $
        sortBy (flip (comparing snd))
               (zip pids views)
  mapM getPostDetails ordered

topics :: Fetch (Map Topic Integer)
topics = do
  posts <- getAllPostsInfo
  return $ Map.fromListWith (+) [(postTopic p, 1 :: Integer) | p <- posts]

-- | Stubbed rendering

data Html = Html String

renderPosts :: [(PostInfo, PostContent)] -> Html
renderPosts _ = Html "renderPost"

renderPage :: Html -> Html -> Html
renderPage _ _ = Html "renderPage"

renderSidePane :: Html -> Html -> Html
renderSidePane _ _ = Html "renderSidePane"

renderTopics :: Map Topic Integer -> Html
renderTopics _ = Html "renderTopics"

renderPostList :: [(PostInfo, PostContent)] -> Html
renderPostList _ = Html "renderPostList"
