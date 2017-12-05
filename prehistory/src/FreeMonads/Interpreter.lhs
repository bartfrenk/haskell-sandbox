> import Data.ByteString (ByteString)

> data CloudFilesF a
>   = SaveFile Path ByteString a
>   | ListFiles Path ([Path] -> a) deriving Functor



The intuition for a free monad of a functor F is a program written in the data
constructors of that functor.

