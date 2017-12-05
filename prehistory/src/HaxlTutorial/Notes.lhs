
> module Notes where


> data Fetch a = Done a | Blocked (Fetch a)

> instance Functor Fetch where
>   fmap f (Done x) = Done (f x)
>   fmap f (Blocked c) = Blocked (fmap f c)

> getChar :: Fetch Char
> getChar = Blocked (Done 'a')

> instance Applicative Fetch where
>   pure = return
>   (<*>) = undefined

> data Request a
>
> dataFetch :: Request a -> Fetch a
> dataFetch = undefined

> instance Monad Fetch where
>   return = Done
>   Done a >>= k = k a
>   Blocked c >>= k = Blocked (c >>= k)

Monad laws

return a >>= f = f a
m >>= return = m
(m >>= f) >>= g = (m >>= (\x -> f x >>= g)

```haskell
return x >>= f
-- = { definition of return }
Done x >>= f
-- = { Done clause of >>= definition }
f x
```

```haskell
Done x >>= return
-- =
return x
-- =
Done x

Blocked c >>= return
-- = { Blocked clause of >>= definition }
Blocked (c >>= return)
-- = { Structural induction }
Blocked c

(Done x >>= f) >>= g
-- =
f x >>= g
-- =
\x -> f x >>= g
-- =
Done x >>= (\x -> f x >>= g)

(Blocked c >>= f) >>= g
-- =
Blocked (c >>= f) >>= g
-- =
Blocked ((c >>= f) >>= g)
-- =
Blocked (c >>= (\x -> f x >>= g))
-- =
Blocked c >>= (\x -> f x >>= g)
```

Monadic ap looks like
```
ap :: m (a -> b) -> m a -> m b
ap mf m = mf >>= (\f -> fmap f m)

app (Done g) (Done y)
-- =
Done g >>= (\f -> Done (f y))
-- =
Done (g y)

app (Done g) (Blocked c)
-- =
Done g >>= (\f -> Blocked (fmap f c))
-- =
Blocked (fmap g c)

app (Blocked c) (Done y)
-- =
Blocked c >>= (\f -> Done (f y))
-- =
Blocked (c >>= (\f -> Done (f y))
-- =
Blocked (app c (Done y))

app (Blocked c) (Blocked d)
-- =
Blocked c >>= (\f -> Blocked (fmap f d))
-- =
Blocked (c >>= (\f -> Blocked (fmap f d))
```
