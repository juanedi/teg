{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Server.PostRedirect where

import GHC.TypeLits
import Servant

type RedirectResponse loc = Headers '[Header "Location" loc] NoContent

type PostRedirect (code :: Nat) loc =
  Verb 'POST code '[JSON] (RedirectResponse loc)

redirect ::
  ToHttpApiData loc =>
  loc ->
  Handler (Headers '[Header "Location" loc] NoContent)
redirect a = return (addHeader a NoContent)
