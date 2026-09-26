-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso hiding (button)
import           Miso.Html.Element as H
import           Miso.Html.Event as E
import           Miso.Html.Property as P
import           Miso.Lens
import           Miso.String (ms)
-----------------------------------------------------------------------------
import           Bulma
-----------------------------------------------------------------------------
data Model = Model
  { _counter       :: Int
  , _modalOpen     :: Bool
  , _dropdownOpen  :: Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \m v -> m { _counter = v }
-----------------------------------------------------------------------------
modalOpen :: Lens Model Bool
modalOpen = lens _modalOpen $ \m v -> m { _modalOpen = v }
-----------------------------------------------------------------------------
dropdownOpen :: Lens Model Bool
dropdownOpen = lens _dropdownOpen $ \m v -> m { _dropdownOpen = v }
-----------------------------------------------------------------------------
data Action
  = Increment
  | Decrement
  | ToggleModal
  | ToggleDropdown
  | NoOp
  deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
-----------------------------------------------------------------------------
app :: App Model Action
app = (component (Model 0 False False) updateModel viewModel)
  { styles = bulmaStylesheet
  }
-----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  Increment      -> counter += 1
  Decrement      -> counter -= 1
  ToggleModal    -> modalOpen %= not
  ToggleDropdown -> dropdownOpen %= not
  NoOp           -> pure ()
-----------------------------------------------------------------------------
viewModel :: Model -> View context props Model Action
viewModel m = H.div_ []
  [ pageNavbar
  , pageHero
  , container [] []
    [ typographySection
    , buttonSection m
    , elementsSection
    , formSection
    , tableSection
    , tagSection
    , breadcrumbSection
    , dropdownSection m
    , cardSection
    , levelSection
    , mediaSection
    , menuSection
    , messageSection
    , paginationSection
    , panelSection
    , tabsSection
    ]
  , modalDemo m
  , pageFooter
  ]
-----------------------------------------------------------------------------
-- Navbar
-----------------------------------------------------------------------------
pageNavbar :: View context props Model Action
pageNavbar =
  navbar [IsPrimary] []
  [ container [] []
    [ navbarBrand [] []
      [ navbarItem [] [P.href_ "#"] [ text "miso-bulma" ]
      ]
    , navbarMenu [] []
      [ navbarStart [] []
        [ navbarItem [] [P.href_ "#"] [ text "Home" ]
        , navbarItem [] [P.href_ "#"] [ text "Documentation" ]
        ]
      , navbarEnd [] []
        [ navbarItemDiv [HasDropdown, IsHoverable] []
          [ navbarLink [] [] [ text "More" ]
          , navbarDropdown [] []
            [ navbarItem [] [P.href_ "#"] [ text "About" ]
            , navbarItem [] [P.href_ "#"] [ text "Contact" ]
            , navbarDivider [] []
            , navbarItem [] [P.href_ "https://bulma.io"] [ text "Bulma.io" ]
            ]
          ]
        ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Hero
-----------------------------------------------------------------------------
pageHero :: View context props Model Action
pageHero =
  hero [IsInfo, IsMedium] []
  [ heroBody [] []
    [ container [HasTextCentered] []
      [ title [] [] [ text "miso-bulma" ]
      , subtitle [] [] [ text "A Bulma component library for Miso" ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Typography
-----------------------------------------------------------------------------
typographySection :: View context props Model Action
typographySection =
  section [] []
  [ title [] [] [ text "Typography" ]
  , H.hr_ []
  , columns [] []
    [ column [] []
      [ title1 [] [] [ text "Title 1" ]
      , title2 [] [] [ text "Title 2" ]
      , title3 [] [] [ text "Title 3" ]
      ]
    , column [] []
      [ subtitle4 [] [] [ text "Subtitle 4" ]
      , subtitle5 [] [] [ text "Subtitle 5" ]
      , subtitle6 [] [] [ text "Subtitle 6" ]
      ]
    ]
  , content []  []
    [ H.p_ [] [ text "This " , H.strong_ [] [text "content"], text " block renders arbitrary prose with Bulma's typographic defaults." ]
    ]
  ]
-----------------------------------------------------------------------------
-- Buttons (interactive counter demo)
-----------------------------------------------------------------------------
buttonSection :: Model -> View context props Model Action
buttonSection m =
  section [] []
  [ title [] [] [ text "Buttons" ]
  , H.hr_ []
  , buttons [] []
    [ button [IsPrimary] [] [ text "Primary" ]
    , button [IsLink] [] [ text "Link" ]
    , button [IsInfo] [] [ text "Info" ]
    , button [IsSuccess] [] [ text "Success" ]
    , button [IsWarning] [] [ text "Warning" ]
    , button [IsDanger] [] [ text "Danger" ]
    , button [IsPrimary, IsOutlined] [] [ text "Outlined" ]
    , button [IsPrimary, IsRounded] [] [ text "Rounded" ]
    , button [IsPrimary, IsLoading] [] [ text "Loading" ]
    , button [IsPrimary, IsLarge] [] [ text "Large" ]
    ]
  , heading [] [] [ text "Live demo" ]
  , level [] []
    [ levelLeft [] []
      [ levelItem [] [] [ button [IsDanger] [ E.onClick Decrement ] [ text "-" ] ]
      , levelItem [] [] [ heading [] [] [ text (ms (_counter m)) ] ]
      , levelItem [] [] [ button [IsPrimary] [ E.onClick Increment ] [ text "+" ] ]
      ]
    ]
  , button [IsInfo, IsLarge] [ E.onClick ToggleModal ] [ text "Open modal" ]
  ]
-----------------------------------------------------------------------------
-- Box / Notification / Progress
-----------------------------------------------------------------------------
elementsSection :: View context props Model Action
elementsSection =
  section [] []
  [ title [] [] [ text "Elements" ]
  , H.hr_ []
  , box [] []
    [ text "A simple box element with a shadow and a border radius." ]
  , notification [IsWarning] []
    [ deleteButton [] [] []
    , text "This is a warning notification."
    ]
  , progress [IsPrimary] [ P.value_ "60", P.max_ "100" ] [ text "60%" ]
  ]
-----------------------------------------------------------------------------
-- Form
-----------------------------------------------------------------------------
formSection :: View context props Model Action
formSection =
  section [] []
  [ title [] [] [ text "Form" ]
  , H.hr_ []
  , field [] []
    [ label [] [] [ text "Name" ]
    , control [] []
      [ input [] [ P.type_ "text", P.placeholder_ "Text input" ]
      ]
    ]
  , field [] []
    [ label [] [] [ text "Department" ]
    , control [] []
      [ selectSpan [] []
        [ H.select_ []
          [ H.option_ [] [ text "Business development" ]
          , H.option_ [] [ text "Marketing" ]
          , H.option_ [] [ text "Sales" ]
          ]
        ]
      ]
    ]
  , field [] []
    [ control [] []
      [ checkboxLabel [] []
        [ checkbox [] [ P.type_ "checkbox" ]
        , text " I agree to the terms and conditions"
        ]
      ]
    ]
  , field [] []
    [ control [] []
      [ radioLabel [] []
        [ radio [] [ P.type_ "radio", P.name_ "answer" ]
        , text " Yes"
        ]
      , radioLabel [] []
        [ radio [] [ P.type_ "radio", P.name_ "answer" ]
        , text " No"
        ]
      ]
    ]
  , field [] []
    [ label [] [] [ text "Message" ]
    , control [] []
      [ textarea [] [ P.placeholder_ "Textarea" ]
      ]
    ]
  , field [] []
    [ control [] []
      [ file [] []
        [ fileLabel [] []
          [ fileInput [] [ P.type_ "file", P.name_ "resume" ]
          , fileCta [] []
            [ fileIcon [] [] [ H.i_ [ P.class_ "fa fa-upload" ] [] ]
            , fileText [] [] [ text "Choose a file…" ]
            ]
          ]
        ]
      ]
    ]
  , field [IsGrouped] []
    [ control [] []
      [ button [IsPrimary] [] [ text "Submit" ]
      ]
    , control [] []
      [ button [] [] [ text "Cancel" ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Table
-----------------------------------------------------------------------------
tableSection :: View context props Model Action
tableSection =
  section [] []
  [ title [] [] [ text "Table" ]
  , H.hr_ []
  , table [IsBordered, IsStriped, IsFullwidth] []
    [ H.thead_ []
      [ H.tr_ []
        [ H.th_ [] [ text "Pos" ]
        , H.th_ [] [ text "Team" ]
        , H.th_ [] [ text "Pts" ]
        ]
      ]
    , H.tbody_ []
      [ H.tr_ []
        [ H.th_ [] [ text "1" ], H.td_ [] [ text "Leicester City" ], H.td_ [] [ text "81" ] ]
      , H.tr_ [ P.class_ "is-selected" ]
        [ H.th_ [] [ text "2" ], H.td_ [] [ text "Arsenal" ], H.td_ [] [ text "71" ] ]
      , H.tr_ []
        [ H.th_ [] [ text "3" ], H.td_ [] [ text "Tottenham Hotspur" ], H.td_ [] [ text "70" ] ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Tags
-----------------------------------------------------------------------------
tagSection :: View context props Model Action
tagSection =
  section [] []
  [ title [] [] [ text "Tags" ]
  , H.hr_ []
  , tags [] []
    [ tag [IsPrimary] [] [ text "Primary" ]
    , tag [IsLink] [] [ text "Link" ]
    , tag [IsInfo] [] [ text "Info" ]
    , tag [IsSuccess] [] [ text "Success" ]
    , tag [IsWarning] [] [ text "Warning" ]
    , tag [IsDanger] [] [ text "Danger" ]
    ]
  ]
-----------------------------------------------------------------------------
-- Breadcrumb
-----------------------------------------------------------------------------
breadcrumbSection :: View context props Model Action
breadcrumbSection =
  section [] []
  [ title [] [] [ text "Breadcrumb" ]
  , H.hr_ []
  , breadcrumb [] []
    [ H.ul_ []
      [ H.li_ [] [ H.a_ [] [ text "Bulma" ] ]
      , H.li_ [] [ H.a_ [] [ text "Documentation" ] ]
      , H.li_ [] [ H.a_ [] [ text "Components" ] ]
      , H.li_ [ P.class_ "is-active" ] [ H.a_ [] [ text "Breadcrumb" ] ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Dropdown (interactive)
-----------------------------------------------------------------------------
dropdownSection :: Model -> View context props Model Action
dropdownSection m =
  section [] []
  [ title [] [] [ text "Dropdown" ]
  , H.hr_ []
  , dropdown (if _dropdownOpen m then [IsActive] else []) []
    [ dropdownTrigger [] []
      [ button [] [ E.onClick ToggleDropdown ]
        [ H.span_ [] [ text "Dropdown button" ]
        , icon [IsSmall] [] [ H.i_ [ P.class_ "fa fa-angle-down" ] [] ]
        ]
      ]
    , dropdownMenu [] []
      [ dropdownContent [] []
        [ dropdownItemA [] [P.href_ "#"] [ text "Dropdown item" ]
        , dropdownItemA [IsActive] [P.href_ "#"] [ text "Active dropdown item" ]
        , dropdownDivider [] []
        , dropdownItemA [] [P.href_ "#"] [ text "With a divider" ]
        ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Card
-----------------------------------------------------------------------------
cardSection :: View context props Model Action
cardSection =
  section [] []
  [ title [] [] [ text "Card" ]
  , H.hr_ []
  , card [] []
    [ cardHeader [] []
      [ cardHeaderTitle [] [] [ text "Component" ]
      , cardHeaderIcon [] [] [ icon [] [] [ H.i_ [ P.class_ "fa fa-angle-down" ] [] ] ]
      ]
    , cardContent [] []
      [ content [] []
        [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit." ]
      ]
    , cardFooter [] []
      [ cardFooterItem [] [] [ text "Save" ]
      , cardFooterItem [] [] [ text "Edit" ]
      , cardFooterItem [] [] [ text "Delete" ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Level
-----------------------------------------------------------------------------
levelSection :: View context props Model Action
levelSection =
  section [] []
  [ title [] [] [ text "Level" ]
  , H.hr_ []
  , level [] []
    [ levelLeft [] []
      [ levelItem [] [] [ H.p_ [] [ H.strong_ [] [ text "123" ], text " posts" ] ]
      ]
    , levelRight [] []
      [ levelItem [] [] [ button [IsPrimary] [] [ text "New" ] ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Media
-----------------------------------------------------------------------------
mediaSection :: View context props Model Action
mediaSection =
  section [] []
  [ title [] [] [ text "Media Object" ]
  , H.hr_ []
  , media [] []
    [ mediaContent [] []
      [ H.p_ [] [ H.strong_ [] [ text "John Smith" ], text " @johnsmith" ]
      , H.p_ [] [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit." ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Menu
-----------------------------------------------------------------------------
menuSection :: View context props Model Action
menuSection =
  section [] []
  [ title [] [] [ text "Menu" ]
  , H.hr_ []
  , menu [] []
    [ menuLabel [] [] [ text "General" ]
    , menuList [] []
      [ H.li_ [] [ H.a_ [] [ text "Dashboard" ] ]
      , H.li_ [] [ H.a_ [] [ text "Customers" ] ]
      ]
    , menuLabel [] [] [ text "Administration" ]
    , menuList [] []
      [ H.li_ [] [ H.a_ [ P.class_ "is-active" ] [ text "Team Settings" ] ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Message
-----------------------------------------------------------------------------
messageSection :: View context props Model Action
messageSection =
  section [] []
  [ title [] [] [ text "Message" ]
  , H.hr_ []
  , message [IsPrimary] []
    [ messageHeader [] []
      [ H.p_ [] [ text "Message" ]
      , deleteButton [] [] []
      ]
    , messageBody [] []
      [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit." ]
    ]
  ]
-----------------------------------------------------------------------------
-- Pagination
-----------------------------------------------------------------------------
paginationSection :: View context props Model Action
paginationSection =
  section [] []
  [ title [] [] [ text "Pagination" ]
  , H.hr_ []
  , pagination [] []
    [ paginationPrevious [] [] [ text "Previous" ]
    , paginationNext [] [] [ text "Next page" ]
    , paginationList [] []
      [ H.li_ [] [ paginationLink [] [] [ text "1" ] ]
      , H.li_ [] [ paginationEllipsis [] [] [ text "…" ] ]
      , H.li_ [] [ paginationLink [IsCurrent] [] [ text "46" ] ]
      , H.li_ [] [ paginationEllipsis [] [] [ text "…" ] ]
      , H.li_ [] [ paginationLink [] [] [ text "86" ] ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Panel
-----------------------------------------------------------------------------
panelSection :: View context props Model Action
panelSection =
  section [] []
  [ title [] [] [ text "Panel" ]
  , H.hr_ []
  , panel [] []
    [ panelHeading [] [] [ text "Repositories" ]
    , panelTabs [] []
      [ H.a_ [ P.class_ "is-active" ] [ text "All" ]
      , H.a_ [] [ text "Public" ]
      , H.a_ [] [ text "Private" ]
      ]
    , panelBlockA [IsActive] []
      [ panelIcon [] [] [ H.i_ [ P.class_ "fa fa-book" ] [] ], text "bulma" ]
    , panelBlockA [] []
      [ panelIcon [] [] [ H.i_ [ P.class_ "fa fa-book" ] [] ], text "miso" ]
    , panelBlock [] []
      [ button [IsPrimary, IsOutlined, IsFullwidth] [] [ text "Reset all filters" ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Tabs
-----------------------------------------------------------------------------
tabsSection :: View context props Model Action
tabsSection =
  section [] []
  [ title [] [] [ text "Tabs" ]
  , H.hr_ []
  , tabs [] []
    [ H.ul_ []
      [ H.li_ [ P.class_ "is-active" ] [ H.a_ [] [ text "Pictures" ] ]
      , H.li_ [] [ H.a_ [] [ text "Music" ] ]
      , H.li_ [] [ H.a_ [] [ text "Videos" ] ]
      , H.li_ [] [ H.a_ [] [ text "Documents" ] ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Modal (interactive)
-----------------------------------------------------------------------------
modalDemo :: Model -> View context props Model Action
modalDemo m =
  modal (if _modalOpen m then [IsActive] else []) []
  [ modalBackground [] [ E.onClick ToggleModal ] []
  , modalCard [] []
    [ modalCardHead [] []
      [ modalCardTitle [] [] [ text "Modal title" ]
      , deleteButton [] [ E.onClick ToggleModal ] []
      ]
    , modalCardBody [] []
      [ text "This modal's visibility is driven by the model, toggled with the "
      , H.code_ [] [ text "ToggleModal" ]
      , text " action."
      ]
    , modalCardFoot [] []
      [ button [IsPrimary] [ E.onClick ToggleModal ] [ text "Save changes" ]
      , button [] [ E.onClick ToggleModal ] [ text "Cancel" ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
-- Footer
-----------------------------------------------------------------------------
pageFooter :: View context props Model Action
pageFooter =
  footer [] []
  [ content [HasTextCentered] []
    [ H.p_ [] [ text "miso-bulma — a Bulma component library for Miso." ]
    ]
  ]
-----------------------------------------------------------------------------
