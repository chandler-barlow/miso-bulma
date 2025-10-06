module Bulma where

import Miso
import Miso.Html.Element as H
import Miso.Html.Property as P

{- |
  The bulma stylesheet from a CDN.
  This stylesheet also includes font awesome for icons as well.
-}
bulmaStylesheet :: _
bulmaStylesheet = 
      [ Style "Bulma"
      , Href "https://jenil.github.io/bulmaswatch/superhero/bulmaswatch.min.css"
      , Href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/7.0.1/css/all.min.css"
      ]


{- |
  These components are designed to be maximally flexible.
  ( ie you can shoot yourself in the foot )

  Most take the form of..

  @@
  component :: [P.Property] -> View action model -> View action model 
  @@

  Where you can pass in any of the properties you want. This means you can break
  the bulma components if you would like to.
-}
