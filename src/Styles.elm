module Styles exposing (..)

import Css exposing (Color, Style, absolute, active, auto, backgroundColor, batch, border3, borderRadius, boxShadow5, boxShadow6, center, checked, color, column, cursor, display, displayFlex, flex, flexDirection, focus, fontSize, fontWeight, height, hex, hidden, hover, inlineBlock, inset, int, justifyContent, lineHeight, listStyleType, margin, marginBottom, marginLeft, marginRight, marginTop, noWrap, none, num, opacity, outline, outline3, overflow, padding2, paddingTop, pct, pointer, position, px, rem, rgba, solid, textAlign, textDecoration, whiteSpace, width)
import Css.Global exposing (adjacentSiblings, typeSelector)



-- Spacing


mr_4 : Style
mr_4 =
    marginRight (rem 1)


mt_4 : Style
mt_4 =
    marginTop (rem 1)


mb_4 : Style
mb_4 =
    marginBottom (rem 1)


mb_2 : Style
mb_2 =
    marginBottom (rem 0.5)


ml_2 : Style
ml_2 =
    marginLeft (rem 0.5)


ml_4 : Style
ml_4 =
    marginLeft (rem 1)


pt_4 : Style
pt_4 =
    paddingTop (rem 1)


w_8 : Style
w_8 =
    width (rem 2)


h_8 : Style
h_8 =
    height (rem 2)


w_full : Style
w_full =
    width (pct 100)


mxAuto : Style
mxAuto =
    batch [ marginLeft auto, marginRight auto ]


pointer_ : Style
pointer_ =
    cursor pointer



-- Display


inline_block : Style
inline_block =
    display inlineBlock


flex_ : Style
flex_ =
    displayFlex


flex_col : Style
flex_col =
    flexDirection column


flex_none : Style
flex_none =
    flex none


justify_center : Style
justify_center =
    justifyContent center



-- Text


text_white : Style
text_white =
    color (hex "#ffffff")


text_success : Style
text_success =
    color (hex "#547a1a")


text_xs : Style
text_xs =
    fontSize (rem 0.75)


text_center : Style
text_center =
    textAlign center


leading_8 : Style
leading_8 =
    lineHeight (rem 2)



-- Color


text_primary : Style
text_primary =
    color (hex "#252525")


text_gray_app_background : Style
text_gray_app_background =
    color (hex "#efefef")


bg_success : Style
bg_success =
    backgroundColor (hex "#547a1a")



-- Misc


rounded_full : Style
rounded_full =
    borderRadius (px 9999)


list_none : Style
list_none =
    listStyleType none



-- Components and non-Tailwind base styles


list__flush : Style
list__flush =
    margin (px 0)


toggle_button : Style
toggle_button =
    batch
        [ opacity (num 0)
        , position absolute
        , focus
            [ adjacentSiblings
                [ typeSelector "label"
                    [ outline3 (px 3) solid (rgba 77 97 171 0.5)
                    ]
                ]
            ]
        , checked
            [ adjacentSiblings
                [ typeSelector "label"
                    [ backgroundColor (hex "#c4dbed")
                    ]
                ]
            ]
        ]


button : Style
button =
    batch
        [ outline none
        , display inlineBlock
        , color (toColor textPrimary)
        , backgroundColor (toColor white)
        , cursor pointer
        , textDecoration none
        , fontSize (rem 0.75)
        , fontWeight (int 500)
        , lineHeight (int 1)
        , borderRadius (px 4)
        , border3 (px 1) solid (toColor borderLight)
        , padding2 (px 13) (px 15)
        , height (px 39)
        , overflow hidden
        , whiteSpace noWrap
        , focus
            [ boxShadow5 (px 0) (px 3) (px 4) (px 0) (rgba 0 0 0 0.6)
            ]
        , hover
            [ backgroundColor (toColor grayLight)
            , textDecoration none
            ]
        , active
            [ backgroundColor (toColor grayMedium)
            , boxShadow6 inset (px 0) (px 2) (px 2) (px 0) (rgba 0 0 0 0.06)
            ]
        ]


type ElementsColor
    = ElementsColor Color


white : ElementsColor
white =
    ElementsColor (hex "#ffffff")


grayLight : ElementsColor
grayLight =
    ElementsColor (hex "#e1e1e1")


grayMedium : ElementsColor
grayMedium =
    ElementsColor (hex "c6c6c6")


textPrimary : ElementsColor
textPrimary =
    ElementsColor (hex "#252525")


borderLight : ElementsColor
borderLight =
    grayLight


toColor : ElementsColor -> Color
toColor (ElementsColor color) =
    color
