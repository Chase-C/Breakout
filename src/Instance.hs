module Instance where

import Types

updateInst :: Time -> Instance -> Instance
updateInst dt inst = inst { iSeconds = iSeconds inst - dt }

iTimeout :: Instance -> Bool
iTimeout inst = iSeconds inst <= 0
