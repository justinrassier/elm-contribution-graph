module DateHelpersTest exposing (..)

import DateHelpers exposing (dayOfYear, dayOfYearToPosix, toMilliseconds, twoDigitString)
import Expect
import Test exposing (..)
import Time exposing (Month(..), Posix, millisToPosix, toDay, utc)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


june29Posix : Posix
june29Posix =
    Time.millisToPosix 1593388800000


december31Posix : Posix
december31Posix =
    Time.millisToPosix 1609445763000


jan1Posix : Time.Posix
jan1Posix =
    Time.millisToPosix 1577836800000


jan1_2021Posix : Time.Posix
jan1_2021Posix =
    Time.millisToPosix 1609459200000


feb1Posix : Time.Posix
feb1Posix =
    Time.millisToPosix 1580515200000


mar31EndOfDayPosix : Time.Posix
mar31EndOfDayPosix =
    Time.millisToPosix 1585695600000


dayChangePosix : Time.Posix
dayChangePosix =
    Time.millisToPosix 1590639893000


utcMinus5 =
    Time.customZone (-5 * 60) []


utcPlus5 =
    Time.customZone (5 * 60) []


all : Test
all =
    describe "The DateHelpers Module"
        [ describe "twoDigitString"
            [ test "returns two digits string with a single digit int for day is passed in" <|
                \_ ->
                    Expect.equal (twoDigitString 1) "01"
            , test "returns two digit string when double digit int for day is passed in" <|
                \_ ->
                    Expect.equal (twoDigitString 12) "12"
            ]
        , describe "getDayOfWeek"
            [ test "returns the correct day for January 1 2020" <|
                \_ ->
                    Expect.equal (DateHelpers.getDayOfWeek Jan 1 2020) 3
            ]
        , describe "getFirstNDigits"
            [ test "returns first two digits of a year" <|
                \_ ->
                    Expect.equal (DateHelpers.firstNDigits 2 2020) 20
            ]
        , describe "toMilliseconds"
            [ test "works for Jan 1 " <|
                \_ ->
                    Expect.equal (toMilliseconds Jan 1 2020) 1577836800000
            , test "works for Feb 29 of a leap year" <|
                \_ ->
                    Expect.equal (toMilliseconds Feb 29 2020) 1582934400000
            ]
        , describe "isLeapYear"
            [ test "true for 2000" <|
                \_ ->
                    Expect.equal (DateHelpers.isLeapYear 2000) True
            , test "true for 2012" <|
                \_ ->
                    Expect.equal (DateHelpers.isLeapYear 2012) True
            , test "false for 2019" <|
                \_ ->
                    Expect.equal (DateHelpers.isLeapYear 2019) False
            ]
        , describe "dayOfYear"
            [ test "returns correct day of the year for 6/29/2020" <|
                \_ -> Expect.equal (dayOfYear utc june29Posix) 181
            , test "returns correct day of the year for 12/31/2020" <|
                \_ -> Expect.equal (dayOfYear utc december31Posix) 366
            , test "returns correct day of the year for 1/1/2020" <|
                \_ -> Expect.equal (dayOfYear utc jan1Posix) 1
            , test "returns correct day when crosses midnight from utc to local" <|
                \_ -> Expect.equal (dayOfYear utcMinus5 dayChangePosix) 148
            , test "returns correct day of the year (Jan 31) when crosses midnight to a previous month due to a negative offset" <|
                \_ -> Expect.equal (dayOfYear utcMinus5 feb1Posix) 31
            , test "returns correct day of tghe year (Apr 1) when crosses midnight to the next month due to a positive offset" <|
                \_ -> Expect.equal (dayOfYear utcPlus5 mar31EndOfDayPosix) 92
            , test "returns correct day  of the year when we cross a year boundary" <|
                \_ -> Expect.equal (dayOfYear utcMinus5 jan1Posix) 365
            , test "returns correct day of the year when we cross a year boundary back into a leap year" <|
                \_ -> Expect.equal (dayOfYear utcMinus5 jan1_2021Posix) 366
            ]
        , describe "dayOfYearToPosix"
            [ test "returns correct posix for 6/29/2020" <|
                \_ -> Expect.equal (dayOfYearToPosix 2020 181) june29Posix
            ]
        ]
