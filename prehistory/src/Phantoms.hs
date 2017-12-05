

data ContentType = ContentType deriving Show
data Param = Param deriving Show
data Part = Part deriving Show

data Payload a = NoPayload
               | Raw ContentType String
               | Params [Param]
               | FormData [Part]
               deriving (Show)
