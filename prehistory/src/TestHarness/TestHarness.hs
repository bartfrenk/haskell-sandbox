module TestHarness where

data Proposition

data ManageF a
  = ListPropositions a

--type ManageAPI a = Free ManageF a

data Market

data Segment

data DynamicF a
  = PushProposition Proposition a

data TurbineF a
  = GetSegments Market ([Segment] -> a)
  | PutSegment Market
