{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bulma where

import Data.Aeson.Types (camelTo2)
import Data.Maybe (listToMaybe)

import Miso.Html.Element
import qualified Miso.Html.Property as P
import Miso.JSON.Types (Value(..))
import Miso.String (MisoString, pack, toMisoString, unwords)
import Miso.Types (Attribute(..), CSS(..), View)

import Prelude hiding (unwords)

{- |
  A stylesheet built from a named theme on
  [Bulmaswatch](https://jenil.github.io/bulmaswatch/), a collection of
  free, third-party Bulma themes (e.g. @"darkly"@, @"cosmo"@,
  @"cyborg"@ - see the Bulmaswatch site for the full list). Also
  includes Font Awesome, for icons.

  Every component in this library only ever emits standard Bulma class
  names, so any Bulma-compatible stylesheet works here - Bulmaswatch,
  the official Bulma build, or a custom Sass build of your own. This
  helper just makes picking a Bulmaswatch theme by name convenient; to
  use something else, build your own @['CSS']@ with 'Href' directly.
-}
bulmaswatchTheme :: MisoString -> [CSS]
bulmaswatchTheme theme =
  [ Href ("https://jenil.github.io/bulmaswatch/" <> theme <> "/bulmaswatch.min.css") False
  , Href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/7.0.1/css/all.min.css" False
  ]

{- |
  The default stylesheet: the @"superhero"@ Bulmaswatch theme. See
  'bulmaswatchTheme' to pick a different one, or supply your own
  @['CSS']@ for a non-Bulmaswatch theme.
-}
bulmaStylesheet :: [CSS]
bulmaStylesheet = bulmaswatchTheme "superhero"

data BulmaModifier =
    IsPrimary
  | IsLink
  | IsInfo
  | IsSuccess
  | IsWarning
  | IsDanger
  | IsDark
  | IsBlack
  | IsWhite
  | IsLight
  | IsText

  | IsCentered
  | IsActive
  | IsCurrent
  | IsSelected
  | IsHovered
  | IsFocused
  | IsHoverable
  | IsTab
  | IsLeft
  | IsRight
  | IsCenter
  | IsMobile

  | IsThreeQuarters
  | IsTwoThirds
  | IsHalf
  | IsOneThird
  | IsOneQuarter

  | IsBordered
  | IsStriped
  | IsNarrow
  | IsHorizontal
  | IsExpanded
  | IsNormal

  | IsSmall
  | IsMedium
  | IsLarge

  | IsOutlined
  | IsInverted
  | IsLoading
  | IsDisabled
  | IsTransparent
  | IsRounded
  | IsDelete

  | IsGrouped
  | IsGapless
  | IsMultiline
  | IsMultiple

  | IsBold
  | IsNarrowMobile
  | IsNarrowTablet
  | IsNarrowDesktop
  | IsHiddenDesktop

  | IsFullheight
  | IsFullwidth

  | IsSquare

  | IsAncestor
  | IsChild
  | IsParent

  | IsBoxed
  | IsToggle
  | IsToggleRounded

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
  | HasTextInfo
  | HasAddons
  | HasIcon
  | HasIconRight
  | HasIconsLeft
  | HasIconsRight
  | HasShadow
  | HasDropdown
  | HasName

  | IsFluid
    deriving (Eq, Show)

addClasses :: forall model action. MisoString -> [BulmaModifier] -> [Attribute model action] -> [Attribute model action]
addClasses baseClass bulmaModifiers as = P.class_ (toMisoString allClasses) : otherAttributes
  where
    newClasses = baseClass : bulmaToText bulmaModifiers

    allClasses :: MisoString
    allClasses = unwords $ case currentClasses of
      Nothing -> newClasses
      Just c  -> c : newClasses

    currentClasses :: Maybe MisoString
    currentClasses = listToMaybe [ v | Property "class" (String v) <- as ]

    otherAttributes :: [Attribute model action]
    otherAttributes = filter (not . isClassProperty) as
      where
        isClassProperty :: Attribute model action -> Bool
        isClassProperty (Property "class" _) = True
        isClassProperty _ = False

bulmaToText :: [BulmaModifier] -> [MisoString]
bulmaToText = map go
  where
    go :: BulmaModifier -> MisoString
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
    go x = pack . camelTo2 '-' . show $ x

-----------------------------------------------------------------------------
-- Grid / Layout
-----------------------------------------------------------------------------

columns :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
columns bms as = div_ (addClasses "columns" bms as)

column :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
column bms as = div_ (addClasses "column" bms as)

container :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
container bms as = div_ (addClasses "container" bms as)

block :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
block bms as = div_ (addClasses "block" bms as)

hero :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
hero bms = section_ . addClasses "hero" bms

heroHead :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
heroHead bms = div_ . addClasses "hero-head" bms

heroBody :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
heroBody bms = div_ . addClasses "hero-body" bms

heroFoot :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
heroFoot bms = div_ . addClasses "hero-foot" bms

section :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
section bms = section_ . addClasses "section" bms

footer :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
footer bms = footer_ . addClasses "footer" bms

tile :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
tile bms = div_ . addClasses "tile" bms

level :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
level bms = nav_ . addClasses "level" bms

levelLeft :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
levelLeft bms = div_ . addClasses "level-left" bms

levelRight :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
levelRight bms = div_ . addClasses "level-right" bms

levelItem :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
levelItem bms = div_ . addClasses "level-item" bms

media :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
media bms = div_ . addClasses "media" bms

articleMedia :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
articleMedia bms = article_ . addClasses "media" bms

mediaContent :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
mediaContent bms = div_ . addClasses "media-content" bms

mediaLeft :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
mediaLeft bms = div_ . addClasses "media-left" bms

mediaRight :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
mediaRight bms = div_ . addClasses "media-right" bms

mediaLeftFigure :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
mediaLeftFigure bms = figure_ . addClasses "media-left" bms

-----------------------------------------------------------------------------
-- Elements
-----------------------------------------------------------------------------

icon :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
icon bms = span_ . addClasses "icon" bms

box :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
box bms = div_ . addClasses "box" bms

button :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
button bms = button_ . addClasses "button" bms

aButton :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
aButton bms = a_ . addClasses "button" bms

buttons :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
buttons bms = div_ . addClasses "buttons" bms

content :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
content bms = div_ . addClasses "content" bms

deleteButton :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
deleteButton bms = button_ . addClasses "delete" bms

heading :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
heading bms = p_ . addClasses "heading" bms

image :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
image bms = figure_ . addClasses "image" bms

link :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
link bms = a_ . addClasses "link" bms

notification :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
notification bms = div_ . addClasses "notification" bms

pNotification :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
pNotification bms = p_ . addClasses "notification" bms

progress :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
progress bms = progress_ . addClasses "progress" bms

table :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
table bms = table_ . addClasses "table" bms

tag :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
tag bms = span_ . addClasses "tag" bms

tags :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
tags bms = div_ . addClasses "tags" bms

title :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
title bms = h1_ . addClasses "title" bms

pTitle :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
pTitle bms = p_ . addClasses "title" bms

subtitle :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
subtitle bms = h2_ . addClasses "subtitle" bms

pSubtitle :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
pSubtitle bms = p_ . addClasses "subtitle" bms

title1 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
title1 bms = h1_ . addClasses "title" (Is1 : bms)

title2 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
title2 bms = h2_ . addClasses "title" (Is2 : bms)

title3 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
title3 bms = h3_ . addClasses "title" (Is3 : bms)

title4 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
title4 bms = h4_ . addClasses "title" (Is4 : bms)

title5 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
title5 bms = h5_ . addClasses "title" (Is5 : bms)

title6 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
title6 bms = h6_ . addClasses "title" (Is6 : bms)

subtitle1 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
subtitle1 bms = h1_ . addClasses "subtitle" (Is1 : bms)

subtitle2 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
subtitle2 bms = h2_ . addClasses "subtitle" (Is2 : bms)

subtitle3 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
subtitle3 bms = h3_ . addClasses "subtitle" (Is3 : bms)

subtitle4 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
subtitle4 bms = h4_ . addClasses "subtitle" (Is4 : bms)

subtitle5 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
subtitle5 bms = h5_ . addClasses "subtitle" (Is5 : bms)

subtitle6 :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
subtitle6 bms = h6_ . addClasses "subtitle" (Is6 : bms)

-----------------------------------------------------------------------------
-- Form
-----------------------------------------------------------------------------

label :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
label bms = label_ . addClasses "label" bms

field :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
field bms = div_ . addClasses "field" bms

fieldLabel :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
fieldLabel bms = div_ . addClasses "field-label" bms

fieldBody :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
fieldBody bms = div_ . addClasses "field-body" bms

help :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
help bms = p_ . addClasses "help" bms

control :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
control bms = div_ . addClasses "control" bms

input :: [BulmaModifier] -> [Attribute model action] -> View context props model action
input bms as = input_ (addClasses "input" bms as)

textarea :: [BulmaModifier] -> [Attribute model action] -> View context props model action
textarea bms as = textarea_ (addClasses "textarea" bms as)

selectSpan :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
selectSpan bms = span_ . addClasses "select" bms

checkboxLabel :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
checkboxLabel bms = label_ . addClasses "checkbox" bms

checkbox :: [BulmaModifier] -> [Attribute model action] -> View context props model action
checkbox bms as = input_ (addClasses "checkbox" bms as)

radioLabel :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
radioLabel bms = label_ . addClasses "radio" bms

radio :: [BulmaModifier] -> [Attribute model action] -> View context props model action
radio bms as = input_ (addClasses "radio" bms as)

file :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
file bms = div_ . addClasses "file" bms

fileLabel :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
fileLabel bms = label_ . addClasses "file-label" bms

fileInput :: [BulmaModifier] -> [Attribute model action] -> View context props model action
fileInput bms as = input_ (addClasses "file-input" bms as)

fileCta :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
fileCta bms = span_ . addClasses "file-cta" bms

fileIcon :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
fileIcon bms = span_ . addClasses "file-icon" bms

fileText :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
fileText bms = span_ . addClasses "file-label" bms

fileName :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
fileName bms = span_ . addClasses "file-name" bms

-----------------------------------------------------------------------------
-- Components
-----------------------------------------------------------------------------

card :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
card bms = div_ . addClasses "card" bms

cardImage :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
cardImage bms = div_ . addClasses "card-image" bms

cardContent :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
cardContent bms = div_ . addClasses "card-content" bms

cardHeader :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
cardHeader bms = header_ . addClasses "card-header" bms

cardHeaderTitle :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
cardHeaderTitle bms = p_ . addClasses "card-header-title" bms

cardHeaderIcon :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
cardHeaderIcon bms = a_ . addClasses "card-header-icon" bms

cardFooter :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
cardFooter bms = footer_ . addClasses "card-footer" bms

cardFooterItem :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
cardFooterItem bms = a_ . addClasses "card-footer-item" bms

breadcrumb :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
breadcrumb bms = nav_ . addClasses "breadcrumb" bms

dropdown :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
dropdown bms = div_ . addClasses "dropdown" bms

dropdownTrigger :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
dropdownTrigger bms = div_ . addClasses "dropdown-trigger" bms

dropdownMenu :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
dropdownMenu bms = div_ . addClasses "dropdown-menu" bms

dropdownContent :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
dropdownContent bms = div_ . addClasses "dropdown-content" bms

dropdownItem :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
dropdownItem bms = div_ . addClasses "dropdown-item" bms

dropdownItemA :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
dropdownItemA bms = a_ . addClasses "dropdown-item" bms

dropdownDivider :: [BulmaModifier] -> [Attribute model action] -> View context props model action
dropdownDivider bms as = hr_ (addClasses "dropdown-divider" bms as)

menu :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
menu bms = aside_ . addClasses "menu" bms

menuLabel :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
menuLabel bms = p_ . addClasses "menu-label" bms

menuList :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
menuList bms = ul_ . addClasses "menu-list" bms

message :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
message bms = article_ . addClasses "message" bms

messageHeader :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
messageHeader bms = div_ . addClasses "message-header" bms

messageBody :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
messageBody bms = div_ . addClasses "message-body" bms

modal :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
modal bms = div_ . addClasses "modal" bms

modalBackground :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
modalBackground bms = div_ . addClasses "modal-background" bms

modalContent :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
modalContent bms = div_ . addClasses "modal-content" bms

modalClose :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
modalClose bms = button_ . addClasses "modal-close" bms

modalCard :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
modalCard bms = div_ . addClasses "modal-card" bms

modalCardHead :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
modalCardHead bms = header_ . addClasses "modal-card-head" bms

modalCardTitle :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
modalCardTitle bms = p_ . addClasses "modal-card-title" bms

modalCardBody :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
modalCardBody bms = section_ . addClasses "modal-card-body" bms

modalCardFoot :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
modalCardFoot bms = footer_ . addClasses "modal-card-foot" bms

navbar :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbar bms = nav_ . addClasses "navbar" bms

navbarBrand :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbarBrand bms = div_ . addClasses "navbar-brand" bms

navbarBurger :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbarBurger bms = a_ . addClasses "navbar-burger" bms

navbarMenu :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbarMenu bms = div_ . addClasses "navbar-menu" bms

navbarStart :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbarStart bms = div_ . addClasses "navbar-start" bms

navbarEnd :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbarEnd bms = div_ . addClasses "navbar-end" bms

navbarItem :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbarItem bms = a_ . addClasses "navbar-item" bms

navbarItemDiv :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbarItemDiv bms = div_ . addClasses "navbar-item" bms

navbarLink :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbarLink bms = div_ . addClasses "navbar-link" bms

navbarDropdown :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
navbarDropdown bms = div_ . addClasses "navbar-dropdown" bms

navbarDivider :: [BulmaModifier] -> [Attribute model action] -> View context props model action
navbarDivider bms as = hr_ (addClasses "navbar-divider" bms as)

pagination :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
pagination bms = nav_ . addClasses "pagination" bms

paginationPrevious :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
paginationPrevious bms = a_ . addClasses "pagination-previous" bms

paginationNext :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
paginationNext bms = a_ . addClasses "pagination-next" bms

paginationList :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
paginationList bms = ul_ . addClasses "pagination-list" bms

paginationLink :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
paginationLink bms = a_ . addClasses "pagination-link" bms

paginationEllipsis :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
paginationEllipsis bms = span_ . addClasses "pagination-ellipsis" bms

panel :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
panel bms = nav_ . addClasses "panel" bms

panelHeading :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
panelHeading bms = p_ . addClasses "panel-heading" bms

panelTabs :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
panelTabs bms = p_ . addClasses "panel-tabs" bms

panelIcon :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
panelIcon bms = span_ . addClasses "panel-icon" bms

panelBlock :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
panelBlock bms = div_ . addClasses "panel-block" bms

panelBlockA :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
panelBlockA bms = a_ . addClasses "panel-block" bms

panelCheckboxLabel :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
panelCheckboxLabel bms = label_ . addClasses "panel-block" bms

tabs :: [BulmaModifier] -> [Attribute model action] -> [View context props model action] -> View context props model action
tabs bms = div_ . addClasses "tabs" bms
