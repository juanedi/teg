module Game.TurnList
  ( TurnList,
    Game.TurnList.init,
    current,
    advance,
    find,
  )
where

import qualified Data.List

data TurnList a = TurnList [a] a [a]
  deriving (Eq)

init :: a -> [a] -> TurnList a
init head tail =
  TurnList [] head tail

current :: TurnList a -> a
current (TurnList _ x _) =
  x

advance :: TurnList a -> TurnList a
advance (TurnList before x after) =
  case (before, after) of
    (_, x' : after') ->
      TurnList (x : before) x' after'
    (x' : before', []) ->
      TurnList [] x' (reverse (x : before'))
    ([], []) ->
      TurnList [] x []

find :: (a -> Bool) -> TurnList a -> Maybe a
find predicate turnList =
  (Data.List.find predicate (toList turnList))
  where
    toList :: TurnList a -> [a]
    toList (TurnList before x after) =
      (reverse before) ++ [x] ++ after
