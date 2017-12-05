# Database.EventStore.Conduit

Work-in-progress. Should allow one to do a monoidal fold over subsequent events
in an event stream, by writing:

```haskell
mappendStream :: (FromJSON a, Monoid a, EventStoreMonad m)
              => StreamName -> Int32 -> Int32 -> m a
mappendStream stream from to =
  let conduit =
        fromStreamBetween stream from to .|
        CL.mapMaybe getJSON .|
        CL.foldMap (\x -> x)
  in runConduit conduit
```
