# Exposing internal functionality (Internal modules)

When a user runs into trouble with using a library the best course of
action usually is to open an issue in the repository or submit a pull
request, but sometimes the user needs things to work *now*.

We admit that we are not perfect and can't think of all use cases for our
library. Sometimes the restrictions we add are too great and may limit
the usage of advanced users that know how things work under the hood and
need certain functionality to use our library.

For that we can expose internal modules to provide some flexibility for
advanced users. Internal modules are not a language concept but
rather a (fairly common) design pattern (or idiom) in Haskell.

Internal modules are simply modules named `Internal`.
These modules are considered risky to use by convention.
Internal modules will export all of the functionality in a
module, and we will add another module without the `Internal` suffix
which will import from our internal module and only export the API
we'd like to export.

Internal modules should be considered unstable and risky to use. If
you end up using one, make sure to open a ticket in the library after
the storm has passed!

We will create a new directory named `Html` and inside it a new file
named `Internal.hs`. The name of this module should be `Html.Internal`.

This module will contain all of the code that was previously in the `Html`
module, but **we will change the module declaration in `Html.Internal`
and _omit_ the export list**:

```hs
-- Html/Internal.hs

module Html.Internal where

...
```

And now in `Html.hs`, we will remove the code that we moved to `Html/Internal.hs`
and in its stead we'll import the internal module:

```hs
-- Hello.hs

module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where
```

Now, users of our library can still import `Html` and safely use our library,
but if they run into trouble and have a dire need to implement unordered lists
to work with our library, they could always work with `Html.Internal` instead.

<details>
  <summary><b>Our revised Html.hs and Html/Internal.hs</b></summary>

```hs
-- Html.hs

module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where

import Html.Internal
```

```hs
-- Html/Internal.hs

module Html.Internal where

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar
```

</details>


