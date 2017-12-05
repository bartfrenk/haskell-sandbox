module Database.EventStore.Event where

import           Data.Conduit
import qualified Data.Conduit.List   as CL
import           Database.EventStore hiding (Event, subscribe)


class FromEvent a where
  fromEvent :: ResolvedEvent -> a

parseEvents :: (Monad m, FromEvent a) => Conduit ResolvedEvent m a
parseEvents = CL.map fromEvent
