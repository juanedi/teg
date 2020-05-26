module Server.Serialization
  ( tagToApiLabel,
    apiLabelToTag,
  )
where

import qualified Text.Casing as Casing

tagToApiLabel :: String -> String
tagToApiLabel constructorTag =
  Casing.toQuietSnake (Casing.fromHumps constructorTag)

apiLabelToTag :: String -> String
apiLabelToTag apiLabel =
  Casing.toPascal (Casing.fromSnake apiLabel)
