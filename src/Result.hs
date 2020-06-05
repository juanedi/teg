module Result
  ( Result (..),
    Error (..),
  )
where

import Data.Text (Text)

{- The result of a computation over a game.

   It can signal an error or emit a value, and allows to modify the resulting
   state regardless of whether we got an error or not.
-}
type Result state result = (state, Either Error result)

data Error
  = InvalidMove Text
  | InternalError Text
