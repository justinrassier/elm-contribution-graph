module DateHelpers exposing (dayOfYear, dayOfYearToPosix, daysInYear, firstNDigits, formatDate, formatTime, getDayOfWeek, isLeapYear, isWithinXDays, monthToStandardValue, monthToString, monthToThreeLetterName, toMilliseconds, twoDigitString, weekdayToInt)

import Time exposing (Month(..), Posix, Weekday(..), posixToMillis, toDay, toHour, toMinute, toMonth, toYear, utc)


formatDate : Time.Zone -> Posix -> String
formatDate zone time =
    monthToString (toMonth zone time) ++ "/" ++ twoDigitString (toDay zone time) ++ "/" ++ String.fromInt (toYear utc time)



-- Note the time is actually going to be wrong for historical data when the time crosses the
-- daylights boundary from where we are now.  Going to maybe have to use ports to get accurate time reporting
--  see https://package.elm-lang.org/packages/elm/time/latest/Time#here for more details


formatTime : Time.Zone -> Posix -> String
formatTime zone time =
    twoDigitString (toHour zone time) ++ ":" ++ twoDigitString (toMinute zone time)


twoDigitString : Int -> String
twoDigitString day =
    "0"
        ++ String.fromInt day
        |> String.right 2


isWithinXDays : Posix -> Int -> Posix -> Bool
isWithinXDays now days dayToCheck =
    dayToCheck
        |> posixToMillis
        |> (<) (posixToMillis now - (60 * 60 * 24 * days * 1000))


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 4 year == 0 && (modBy 100 year /= 0 || modBy 400 year == 0)


daysInYear : Int -> Int
daysInYear year =
    if isLeapYear year then
        366

    else
        365



-- http://howardhinnant.github.io/date_algorithms.html#days_from_civil


toMilliseconds : Month -> Int -> Int -> Int
toMilliseconds month day year =
    let
        yearModified =
            if monthToStandardValue month <= 2 then
                year - 1

            else
                year

        era : Int
        era =
            let
                yearModification =
                    if yearModified >= 0 then
                        year

                    else
                        yearModified - 399
            in
            yearModification // 400

        yoe : Int
        yoe =
            yearModified - era * 400

        doy : Int
        doy =
            let
                monthVal =
                    monthToStandardValue month

                monthModification =
                    if monthVal > 2 then
                        -3

                    else
                        9
            in
            (153 * (monthVal + monthModification) + 2) // 5 + day - 1

        doe : Int
        doe =
            yoe * 365 + yoe // 4 - yoe // 100 + doy
    in
    (era * 146097 + doe - 719468) * 86400000


dayOfYear : Time.Zone -> Posix -> Int
dayOfYear timeZone posix =
    let
        -- midnight to midnight of a single day is an exact calulation of a day. Jan 2 at midnight will round to 1, even though it officially
        -- starts the second day. So move the calc back a millisecond as to force a rounding
        oneMillisecondBeforeMidnight =
            toMilliseconds Jan 1 (Time.toYear timeZone posix) - 1

        dayDifferenceFromUTC =
            Time.toDay utc posix - Time.toDay timeZone posix

        -- if we cross a month boundary, we want to flip the offset back since sutraction caused the polarity to flip
        offset =
            if Basics.abs dayDifferenceFromUTC > 1 then
                -1 * clamp -1 1 dayDifferenceFromUTC

            else
                dayDifferenceFromUTC

        millis =
            Time.posixToMillis posix

        diff =
            millis - oneMillisecondBeforeMidnight
    in
    ceiling (toFloat diff / 86400000) - offset


dayOfYearToPosix : Int -> Int -> Time.Posix
dayOfYearToPosix year doy =
    let
        firstOfYear =
            toMilliseconds Jan 1 year

        millis =
            firstOfYear + ((doy - 1) * 86400000)
    in
    Time.millisToPosix millis



-- Date calculation taken from https://cs.uwaterloo.ca/~alopez-o/math-faq/node73.html


getDayOfWeek : Month -> Int -> Int -> Int
getDayOfWeek month day year =
    let
        century =
            firstNDigits 2 year

        monthVal =
            monthToDayOfWeekCalculationValue month

        --  last two digits of the year. Jan and Feb are treated as the previous year
        yearVal =
            case month of
                Jan ->
                    modBy 100 (year - 1)

                Feb ->
                    modBy 100 (year - 1)

                _ ->
                    modBy 100 year
    in
    (day + floor (2.6 * toFloat monthVal - 0.2) - 2 * century + yearVal + floor (toFloat yearVal / 4) + floor (toFloat century / 4)) |> modBy 7


firstNDigits : Int -> Int -> Int
firstNDigits n num =
    num // (10 ^ (truncate (logBase 10 (toFloat num)) - n + 1))


monthToStandardValue : Month -> Int
monthToStandardValue month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12



-- Treat jan and feb as 11 and 12 of the previous year to make the calculation work


monthToDayOfWeekCalculationValue : Month -> Int
monthToDayOfWeekCalculationValue month =
    case month of
        Jan ->
            11

        Feb ->
            12

        Mar ->
            1

        Apr ->
            2

        May ->
            3

        Jun ->
            4

        Jul ->
            5

        Aug ->
            6

        Sep ->
            7

        Oct ->
            8

        Nov ->
            9

        Dec ->
            10


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


monthToThreeLetterName : Month -> String
monthToThreeLetterName month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


weekdayToInt : Time.Weekday -> Int
weekdayToInt weekday =
    case weekday of
        Sun ->
            0

        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6
