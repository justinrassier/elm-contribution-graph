module ContributionGraph exposing (view, update, Config, Contribution, Model, Msg, init, setContributions)

{-| This module creates a GitHub-style contribution graph based on a simple Contribution model. The original
intent is to have leaderboard, so each contribution has a point value associated.

@docs view, update, Config, Contribution, Model, Msg, init, setContributions

-}

import DateHelpers exposing (formatDate)
import Html exposing (Html, a, div, input, label, li, span, text, ul)
import Html.Attributes exposing (checked, class, for, href, id, name, target, title, type_, value)
import Html.Events exposing (onInput)
import Platform.Cmd exposing (Cmd)
import Svg
import Svg.Attributes
import Svg.Events exposing (onClick)
import Task
import Time exposing (Month(..), Weekday(..), utc)


{-| Opaque data model that holds the internal state of the Contribution Graph
-}
type Model
    = Model InternalModel


{-| Messages that the Contribution Graph responds to update its internal state
-}
type Msg
    = OnClick (List Contribution)
    | OnYearChange String
    | GotTimeZone Time.Zone


{-| Config options that are specific to your provider and your repo

Example:

    config =
        { repoCommitUrl = "https://github.com/justinrassier/justin-rassier-gatsby/commit/"
        }

-}
type alias Config =
    { repoCommitUrl : String
    }


{-| Contribution data model used to display data in the graph.
-}
type alias Contribution =
    { mergeTimestamp : Time.Posix
    , title : String
    , commitSha : String
    , points : Int
    }


type alias InternalModel =
    { allContributions : List Contribution
    , selectedContributions : List Contribution
    , timeZone : Time.Zone
    , year : Int
    , config : Config
    }


{-| Initialize the [`Model`](#Model), which is an opaque type that controls the internal state of the graph
-}
init : Config -> List Contribution -> ( Model, Cmd Msg )
init config contributions =
    ( Model
        { allContributions = contributions
        , selectedContributions = []
        , timeZone = utc
        , year = defaultYear contributions utc
        , config = config
        }
    , Task.perform GotTimeZone Time.here
    )


{-| Setter for the list of Contributions in the Model to render
-}
setContributions : List Contribution -> Model -> Model
setContributions allContributions (Model model) =
    Model
        { model
            | allContributions = allContributions
            , year = defaultYear allContributions model.timeZone
        }


formatTitle : String -> String
formatTitle title =
    String.lines title
        |> List.head
        |> Maybe.withDefault "<no title>"


defaultYear : List Contribution -> Time.Zone -> Int
defaultYear contributions zone =
    contributions
        |> List.map .mergeTimestamp
        |> List.map (Time.toYear zone)
        |> List.maximum
        |> Maybe.withDefault 2020


{-| Standard update function that handles Contribution Graph interactions. Hook up to your update function using [`Cmd.map`](#Platform.Cmd.map)
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        OnClick selectedContributions ->
            ( Model { model | selectedContributions = selectedContributions }, Cmd.none )

        OnYearChange newYear ->
            let
                convertedYear =
                    Maybe.withDefault model.year (String.toInt newYear)
            in
            ( Model { model | year = convertedYear, selectedContributions = [] }, Cmd.none )

        GotTimeZone timeZone ->
            ( Model { model | timeZone = timeZone }, Cmd.none )


{-| View function to render contribution graph as an SVG
-}
view : Model -> Html Msg
view (Model model) =
    let
        monthTextElements : List (Svg.Svg msg)
        monthTextElements =
            [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
                |> List.map viewSvgMonthText

        dayTextElements =
            [ Svg.text_ [ Svg.Attributes.textAnchor "start", Svg.Attributes.dy "50" ] [ Svg.text "Mon" ]
            , Svg.text_ [ Svg.Attributes.textAnchor "start", Svg.Attributes.dy "81" ] [ Svg.text "Wed" ]
            , Svg.text_ [ Svg.Attributes.textAnchor "start", Svg.Attributes.dy "111" ] [ Svg.text "Fri" ]
            ]

        yearList : List Int
        yearList =
            model.allContributions
                |> List.map .mergeTimestamp
                |> List.map (Time.toYear model.timeZone)

        yearsToDisplay : List Int
        yearsToDisplay =
            List.range
                (List.minimum yearList |> Maybe.withDefault model.year)
                (List.maximum yearList |> Maybe.withDefault model.year)
    in
    div []
        [ div []
            [ div [ class "flex" ]
                (yearsToDisplay
                    |> List.map
                        (\y ->
                            div [ class "mr-4" ]
                                [ input
                                    [ id (String.fromInt y)
                                    , type_ "radio"
                                    , name "year-select"
                                    , class "toggle-button"
                                    , checked (model.year == y)
                                    , onInput OnYearChange
                                    , value (String.fromInt y)
                                    ]
                                    []
                                , label [ class "button", for (String.fromInt y) ] [ text (String.fromInt y) ]
                                ]
                        )
                )
            , div []
                [ Svg.svg
                    [ Svg.Attributes.width "900"
                    , Svg.Attributes.height "150"
                    , Svg.Attributes.class
                        "display-block mx-auto mt-4"
                    ]
                    [ Svg.g []
                        (List.concat
                            [ [ viewSvgYear model.timeZone
                                    model.year
                                    model.allContributions
                              ]
                            , monthTextElements
                            , dayTextElements
                            ]
                        )
                    ]
                ]
            , div []
                [ span []
                    [ text
                        (model.selectedContributions
                            |> List.head
                            |> Maybe.map .mergeTimestamp
                            |> Maybe.map (DateHelpers.formatDate model.timeZone)
                            |> Maybe.withDefault ""
                        )
                    ]
                , ul
                    [ class "list list-none list--flush p-4" ]
                    (model.selectedContributions
                        |> List.map
                            (\c ->
                                li []
                                    [ div [ class "flex" ]
                                        [ div [ class "flex-none w-8 " ]
                                            [ div [ class "text-white text-xs bg-success rounded-full w-8 h-8" ]
                                                [ span [ class "inline-block leading-8 text-center w-full" ] [ text ("+" ++ String.fromInt c.points) ]
                                                ]
                                            ]
                                        , div [ class "flex flex-col justify-center" ]
                                            [ a
                                                [ class "pointer ml-4"
                                                , target "_blank"
                                                , title (formatDate model.timeZone c.mergeTimestamp)
                                                , href (model.config.repoCommitUrl ++ c.commitSha)
                                                ]
                                                (viewTitle
                                                    c.title
                                                )
                                            ]
                                        ]
                                    ]
                            )
                    )
                ]
            ]
        ]


viewTitle : String -> List (Html msg)
viewTitle title =
    [ div [ class "ml-2" ]
        [ text (formatTitle title)
        ]
    ]


viewSvgMonthText : Month -> Svg.Svg msg
viewSvgMonthText month =
    Svg.text_
        [ Svg.Attributes.y
            "10"
        , Svg.Attributes.x (String.fromInt (DateHelpers.monthToStandardValue month * 68))
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.class "text-primary"
        ]
        [ Svg.text (DateHelpers.monthToThreeLetterName month) ]


viewSvgYear : Time.Zone -> Int -> List Contribution -> Svg.Svg Msg
viewSvgYear timeZone year contributions =
    Svg.g [ Svg.Attributes.transform "translate(20,25)" ]
        (List.range 0 52
            |> List.map (\v -> viewSvgWeek timeZone year v contributions)
        )


viewSvgWeek : Time.Zone -> Int -> Int -> List Contribution -> Svg.Svg Msg
viewSvgWeek timeZone year weekOfYear contributions =
    let
        calendarStartDayOfWeek =
            DateHelpers.getDayOfWeek Jan 1 year

        weekStartDay =
            (weekOfYear * 7) - calendarStartDayOfWeek

        weekEndDay =
            weekStartDay + 7

        -- drop the remainder of the days over 365/366 plus the few extra days needed based on when Jan 1 was in the week
        daysToTake =
            if weekOfYear == 52 then
                7 - (weekEndDay - DateHelpers.daysInYear year)

            else
                7

        -- if it's the first week, drop the first cells until we hit the start day of the week. This is offset at the end of the year
        daysToDrop =
            if weekOfYear == 0 then
                calendarStartDayOfWeek

            else
                0

        doy : Time.Weekday -> Int
        doy weekDay =
            (DateHelpers.weekdayToInt weekDay + 1) + weekStartDay
    in
    Svg.g [ Svg.Attributes.transform ("translate(" ++ String.fromInt (weekOfYear * 16) ++ ", 0)") ]
        ([ viewSvgDay year (doy Sun) (getContributionsForDayOfYear timeZone year (doy Sun) contributions)
         , viewSvgDay year (doy Mon) (getContributionsForDayOfYear timeZone year (doy Mon) contributions)
         , viewSvgDay year (doy Tue) (getContributionsForDayOfYear timeZone year (doy Tue) contributions)
         , viewSvgDay year (doy Wed) (getContributionsForDayOfYear timeZone year (doy Wed) contributions)
         , viewSvgDay year (doy Thu) (getContributionsForDayOfYear timeZone year (doy Thu) contributions)
         , viewSvgDay year (doy Fri) (getContributionsForDayOfYear timeZone year (doy Fri) contributions)
         , viewSvgDay year (doy Sat) (getContributionsForDayOfYear timeZone year (doy Sat) contributions)
         ]
            --takes only a partial weekOfYear on the last weekOfYear of the year
            |> List.take daysToTake
            -- drops off the first few days if the start day is not on Sundy
            |> List.drop daysToDrop
        )


viewSvgDay : Int -> Int -> List Contribution -> Svg.Svg Msg
viewSvgDay year dayOfYear contributions =
    let
        dayOfWeek =
            dayOfYear
                |> DateHelpers.dayOfYearToPosix year
                |> Time.toWeekday utc
                |> DateHelpers.weekdayToInt

        yOffset : String
        yOffset =
            String.fromInt (15 * dayOfWeek)

        fill : String
        fill =
            if List.length contributions > 0 then
                "text-success cursor-pointer"

            else
                "text-gray-app-background"

        date =
            DateHelpers.dayOfYearToPosix year dayOfYear
                |> DateHelpers.formatDate utc
    in
    Svg.rect
        [ Svg.Attributes.id date
        , Svg.Attributes.width "11"
        , Svg.Attributes.height "11"
        , Svg.Attributes.x "16"
        , Svg.Attributes.y yOffset
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.class fill
        , onClick (OnClick contributions)
        ]
        [ Svg.title [] [ Svg.text date ] ]


getContributionsForDayOfYear : Time.Zone -> Int -> Int -> List Contribution -> List Contribution
getContributionsForDayOfYear timeZone year dayOfYear contributions =
    contributions
        |> List.map (\v -> ( v.mergeTimestamp, v ))
        |> List.filter (\( timestamp, _ ) -> Time.toYear timeZone timestamp == year)
        |> List.filter (\( timestamp, _ ) -> DateHelpers.dayOfYear timeZone timestamp == dayOfYear)
        |> List.map Tuple.second
