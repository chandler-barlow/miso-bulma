{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Bulma where

import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (div)

import Miso.Html
import qualified Miso.Html as Html
import qualified Miso.Html.Property as P

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

data BulmaModifier =
    IsPrimary
  | IsInfo
  | IsSuccess
  | IsWarning
  | IsDanger
  | IsDark

  | IsCentered
  | IsActive
  | IsTab
  | IsLeft
  | IsRight
  | IsCenter

  | IsThreeQuarters
  | IsTwoThirds
  | IsHalf
  | IsOneThird
  | IsOneQuarter

  | IsBordered
  | IsStriped
  | IsNarrow

  | IsSmall
  | IsMedium
  | IsLarge

  | IsOutlined
  | IsInverted
  | IsLoading
  | IsDisabled

  | IsGrouped
  | IsGapless
  | IsMultline

  | IsBold
  | IsNarrowMobile
  | IsNarrowTablet
  | IsNarrowDesktop

  | IsFullheight
  | IsFullwidth

  | IsSquare

  | IsAncestor
  | IsChild
  | IsParent

  | IsBoxed
  | IsToggle

  | Notification
  | NavMenu
  | Help

  | Is1
  | Is2
  | Is3
  | Is4
  | Is5
  | Is6
  | Is7
  | Is8
  | Is9
  | Is10
  | Is11

  | Is1By1
  | Is2By1
  | Is3By2
  | Is16By9
  | Is4By3

  | Is16By16
  | Is24By24
  | Is32By32
  | Is48By48
  | Is64By64
  | Is96By96
  | Is128By128

  | HasTextCentered
  | HasAddons
  | HasIcon
  | HasIconRight
  | HasShadow

  | IsFluid
    deriving (Eq, Show)

addClasses :: Text -> [BulmaModifier] -> [P.Attribute] -> [P.Attribute]
addClasses classNames bulmaModifiers as = as'
  where
    newClasses = map (,True) $ classNames : bulmaToText bulmaModifiers

    as' = case currentClasses of
      Nothing -> classList newClasses : removedClass
      Just c  -> classList ((c, True) : newClasses) : removedClass

    currentClasses :: Maybe Text
    currentClasses = listToMaybe [ v | KV _ "class" v <- as ]

    removedClass :: [P.Attribute]
    removedClass = filter go as
      where
        go :: P.Attribute -> Bool
        go (KV _ "class" _) = False
        go _ = True

bulmaToText :: [BulmaModifier] -> [Text]
bulmaToText = map go
  where
    go :: BulmaModifier -> Text
    go Is1 = "is-1"
    go Is2 = "is-2"
    go Is3 = "is-3"
    go Is4 = "is-4"
    go Is5 = "is-5"
    go Is6 = "is-6"
    go Is7 = "is-7"
    go Is8 = "is-8"
    go Is9 = "is-9"
    go Is10 = "is-10"
    go Is11 = "is-11"
    go Is1By1 = "is-1by1"
    go Is2By1 = "is-2by1"
    go Is3By2 = "is-3by2"
    go Is16By9 = "is-16by9"
    go Is4By3 = "is-4by3"
    go Is16By16 = "is-16x16"
    go Is24By24 = "is-24x24"
    go Is32By32 = "is-32x32"
    go Is48By48 = "is-48x48"
    go Is64By64 = "is-64x64"
    go Is96By96 = "is-96x96"
    go Is128By128 = "is-128x128"
    go x = T.pack . camelTo2 '-' . show $ x

-- Grid
columns :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
columns bms as = div (addClasses "columns" bms as)

column :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
column bms as = div (addClasses "column" bms as)

container :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
container bms as = Html.div (addClasses "container" bms as)

hero :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
hero bms = Html.section . addClasses "hero" bms

heroHead :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
heroHead bms = div . addClasses "hero-head" bms

heroBody :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
heroBody bms = div . addClasses "hero-body" bms

heroFoot :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
heroFoot bms = div . addClasses "hero-foot" bms

section :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
section bms = Html.section . addClasses "section" bms

footer :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
footer bms = Html.footer . addClasses "footer" bms

-- Components

card :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
card bms = Html.div . addClasses "card" bms

cardImage :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
cardImage bms = Html.div . addClasses "card-image" bms

cardContent :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
cardContent bms = Html.div . addClasses "card-content" bms

cardHeader :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
cardHeader bms = Html.div . addClasses "card-header" bms

cardHeaderTitle :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
cardHeaderTitle bms = Html.p . addClasses "card-header-title" bms

cardHeaderIcon :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
cardHeaderIcon bms = Html.a . addClasses "card-header-icon" bms

cardFooter :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
cardFooter bms = Html.a . addClasses "card-footer" bms

cardFooterItem :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
cardFooterItem bms = Html.a . addClasses "card-footer-item" bms

media :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
media bms = Html.div . addClasses "media" bms

articleMedia :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
articleMedia bms = Html.article . addClasses "media" bms

mediaContent :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
mediaContent bms = Html.div . addClasses "media-content" bms

mediaLeft :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
mediaLeft bms = Html.div . addClasses "media-left" bms

mediaLeftFigure :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
mediaLeftFigure bms = Html.figure . addClasses "media-left" bms

level :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
level bms = Html.nav . addClasses "level" bms

levelLeft :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
levelLeft bms = Html.div . addClasses "level-left" bms

levelRight :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
levelRight bms = Html.div . addClasses "level-right" bms

levelItem :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
levelItem bms = Html.div . addClasses "level-item" bms

-- elements
icon :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
icon bms = Html.span . addClasses "icon" bms

box :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
box bms = Html.div . addClasses "box" bms

label :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
label bms = Html.label . addClasses "label" bms

selectSpan :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
selectSpan bms = Html.span . addClasses "select" bms

checkboxLabel :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
checkboxLabel bms = Html.label . addClasses "checkbox" bms

checkbox :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
checkbox bms = Html.input . addClasses "checkbox" bms

radioLabel :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
radioLabel bms = Html.label . addClasses "radio" bms

radio :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
radio bms = Html.input . addClasses "radio" bms

tag :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
tag bms = Html.span . addClasses "tag" bms

tile :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
tile bms = Html.div . addClasses "tile" bms

textarea :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
textarea bms = Html.p . addClasses "textarea" bms

control :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
control bms = Html.div . addClasses "control" bms

controlLabel :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
controlLabel bms = Html.div . addClasses "control-label" bms

image :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
image bms = Html.figure . addClasses "image" bms

link :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
link bms = Html.a . addClasses "link" bms

button :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
button bms = Html.button . addClasses "button" bms

aButton :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
aButton bms = Html.a . addClasses "button" bms

input :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
input bms = Html.input . addClasses "input" bms

content :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
content bms = Html.div . addClasses "content" bms

heading :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
heading bms = div . addClasses "heading" bms

title :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
title bms = h1 . addClasses "title" bms

notification :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
notification bms = div . addClasses "notification" bms

pNotification :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
pNotification bms = p . addClasses "notification" bms

deleteButton :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
deleteButton bms = Html.button . addClasses "delete" bms

progress :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
progress bms = Html.progress . addClasses "progress" bms

pTitle :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
pTitle bms = p . addClasses "title" bms

subtitle :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
subtitle bms = h2 . addClasses "subtitle" bms

table :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
table bms = h2 . addClasses "table" bms

pSubtitle :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
pSubtitle bms = p . addClasses "subtitle" bms

tabs :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
tabs bms = div . addClasses "tabs" bms

navPanel :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
navPanel bms = Html.nav . addClasses "panel" bms

panel :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
panel bms = div . addClasses "panel" bms

panelHeading :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
panelHeading bms = p . addClasses "panel-heading" bms

panelTabs :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
panelTabs bms = p . addClasses "panel-tabs" bms

panelIcon :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
panelIcon bms = Html.span . addClasses "panel-icon" bms

panelBlock :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
panelBlock bms = div . addClasses "panel-block" bms

panelBlockA :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
panelBlockA bms = a . addClasses "panel-block" bms

panelCheckboxLabel :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
panelCheckboxLabel bms = Html.label . addClasses "panel-checkbox" bms

pagination :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
pagination bms = Html.nav . addClasses "pagination" bms

nav :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
nav bms = Html.nav . addClasses "nav" bms

navLeft :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
navLeft bms = Html.div . addClasses "nav-left" bms

navCenter :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
navCenter bms = Html.div . addClasses "nav-center" bms

navRight :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
navRight bms = Html.div . addClasses "nav-right" bms

navItem :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
navItem bms = Html.a . addClasses "nav-item" bms

navToggle :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
navToggle bms = Html.span . addClasses "nav-toggle" bms

modal :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
modal bms = Html.div . addClasses "modal" bms

modalBackground :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
modalBackground bms = Html.div . addClasses "modal-background" bms

modalContainer :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
modalContainer bms = Html.div . addClasses "modal-container" bms

modalContent :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
modalContent bms = Html.div . addClasses "modal-content" bms

modalClose :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
modalClose bms = Html.button . addClasses "modal-close" bms

modalCard :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
modalCard bms = Html.div . addClasses "modal-card" bms

modalCardHead :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
modalCardHead bms = Html.header . addClasses "modal-card-head" bms

modalCardFoot :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
modalCardFoot bms = Html.footer . addClasses "modal-card-foot" bms

modalCardTitle :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
modalCardTitle bms = Html.p . addClasses "modal-card-title" bms

message :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
message bms = Html.article . addClasses "message" bms

messageHeader :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
messageHeader bms = Html.article . addClasses "message-header" bms

messageBody :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
messageBody bms = Html.article . addClasses "message-body" bms

menu :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
menu bms = Html.aside . addClasses "menu" bms

menuLabel :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
menuLabel bms = Html.p . addClasses "menu-label" bms

menuList :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
menuList bms = Html.ul . addClasses "menu-list" bms

title1 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
title1 bms = Html.h1 . addClasses "title" (Is1 : bms)

title2 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
title2 bms = Html.h2 . addClasses "title" (Is2 : bms)

title3 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
title3 bms = Html.h3 . addClasses "title" (Is3 : bms)

title4 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
title4 bms = Html.h4 . addClasses "title" (Is4 : bms)

title5 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
title5 bms = Html.h5 . addClasses "title" (Is5 : bms)

title6 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
title6 bms = Html.h6 . addClasses "title" (Is6 : bms)

subtitle1 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
subtitle1 bms = Html.h1 . addClasses "subtitle" (Is1 : bms)

subtitle2 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
subtitle2 bms = Html.h2 . addClasses "subtitle" (Is2 : bms)

subtitle3 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
subtitle3 bms = Html.h3 . addClasses "subtitle" (Is3 : bms)

subtitle4 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
subtitle4 bms = Html.h4 . addClasses "subtitle" (Is4 : bms)

subtitle5 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
subtitle5 bms = Html.h5 . addClasses "subtitle" (Is5 : bms)

subtitle6 :: [BulmaModifier] -> [P.Attribute] -> [View action model] -> View action model
subtitle6 bms = Html.h6 . addClasses "subtitle" (Is6 : bms)
