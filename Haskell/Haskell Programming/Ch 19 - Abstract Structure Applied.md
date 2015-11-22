# Abstract Structure Applied

## Monoid

Monodis are everywhere once you recognize the pattern and start looking for them. 

### Templating content in Scotty

Here the Scotty web framework's Hello World example uses `mconca` to inject the parameter "word" into the HTML page returned:

```
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Monoid (mconcat)

main = scotty 3000 $ do
	get "/:word" $ do
	beam <- param "word"
	html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

