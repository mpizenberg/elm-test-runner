module ElmTestRunner.Reporter.ConsoleBis exposing (implementation)

import Array exposing (Array)
import ElmTestRunner.Failure exposing (Failure)
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (Summary, TestResult(..))
import ElmTestRunner.SeededRunners exposing (Kind(..))
import ElmTestRunner.Vendor.ConsoleFormat exposing (format)
import ElmTestRunner.Vendor.ConsoleText as Text exposing (..)
import ElmTestRunner.Vendor.FormatColor as FormatColor
import ElmTestRunner.Vendor.FormatMonochrome as FormatMonochrome
import Test.Runner exposing (formatLabels)


{-| Provide a console implementation of a reporter, mostly for human consumption.
Require the initial random seed and number of fuzz runs.
-}
implementation : { seed : Int, fuzzRuns : Int } -> Interface
implementation options =
    { onBegin = onBegin options
    , onResult = onResult
    , onEnd = onEnd
    }


onBegin : { seed : Int, fuzzRuns : Int } -> Int -> Maybe String
onBegin { seed, fuzzRuns } testsCount =
    """
Running {{ testsCount }} tests. To reproduce these results later, run:
elm-test-rs --seed {{ seed }} --fuzz {{ fuzzRuns }} {{ files }}

"""
        |> String.replace "{{ testsCount }}" (String.fromInt testsCount)
        |> String.replace "{{ seed }}" (String.fromInt seed)
        |> String.replace "{{ fuzzRuns }}" (String.fromInt fuzzRuns)
        |> String.replace "{{ files }}" "(TODO: pass files to reporter)"
        |> Just



--


onResult : TestResult -> Maybe String
onResult testResult =
    let
        useColor =
            -- Text.Monochrome
            Text.UseColor
    in
    case testResult of
        Passed _ ->
            Nothing

        Failed { labels, todos, failures, logs } ->
            if List.isEmpty todos then
                -- We have non-TODOs still failing; report them, not the TODOs.
                failuresToText useColor labels failures
                    |> Text.render useColor
                    |> Just

            else
                List.map (\todo -> formatTodo labels todo ++ "\n") todos
                    |> String.concat
                    |> Just


formatTodo : List String -> String -> String
formatTodo labels todo =
    String.concat
        [ "◦ "
        , String.concat (List.map (\l -> l ++ " → ") labels)
        , "TODO: "
        , todo
        ]


failuresToText : UseColor -> List String -> List Failure -> Text
failuresToText useColor labels failures =
    Text.concat (failureLabelsToText labels :: List.map (failureToText useColor) failures)


failureLabelsToText : List String -> Text
failureLabelsToText =
    formatLabels (dark << plain << withChar '↓') (red << withChar '✗') >> Text.concat


failureToText : UseColor -> Failure -> Text
failureToText useColor { given, description, reason } =
    let
        formatEquality =
            case useColor of
                Monochrome ->
                    FormatMonochrome.formatEquality

                UseColor ->
                    FormatColor.formatEquality

        messageText =
            plain ("\n" ++ indent (format formatEquality description reason) ++ "\n\n")
    in
    case given of
        Nothing ->
            messageText

        Just givenStr ->
            [ dark (plain ("\nGiven " ++ givenStr ++ "\n"))
            , messageText
            ]
                |> Text.concat


indent : String -> String
indent str =
    String.split "\n" str
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"



--


onEnd : Result String Kind -> Array TestResult -> Maybe String
onEnd kindResult testResults =
    case kindResult of
        Err err ->
            Just ("Your tests are invalid: " ++ err ++ "\n")

        Ok kind ->
            formatSummary kind (TestResult.summary testResults)
                |> Just


formatSummary : Kind -> Summary -> String
formatSummary kind { totalDuration, passedCount, failedCount, todoCount } =
    let
        useColor =
            -- Text.Monochrome
            Text.UseColor

        headlineResult =
            case ( kind, failedCount, todoCount ) of
                ( Plain, 0, 0 ) ->
                    Ok "TEST RUN PASSED"

                ( Plain, 0, 1 ) ->
                    Err ( yellow, "TEST RUN INCOMPLETE", " because there is 1 TODO remaining" )

                ( Plain, 0, numTodos ) ->
                    Err ( yellow, "TEST RUN INCOMPLETE", " because there are " ++ String.fromInt numTodos ++ " TODOs remaining" )

                ( Only, 0, _ ) ->
                    Err ( yellow, "TEST RUN INCOMPLETE", " because Test.only was used" )

                ( Skipping, 0, _ ) ->
                    Err ( yellow, "TEST RUN INCOMPLETE", " because Test.skip was used" )

                _ ->
                    Err ( red, "TEST RUN FAILED", "" )

        headline =
            case headlineResult of
                Ok str ->
                    underline (green ("\n" ++ str ++ "\n\n"))

                Err ( colorize, str, suffix ) ->
                    [ underline (colorize ("\n" ++ str))
                    , colorize (suffix ++ "\n\n")
                    ]
                        |> Text.concat

        todoStats =
            -- Print stats for Todos if there are any,
            --but don't print details unless only Todos remain
            if todoCount == 0 then
                plain ""

            else
                stat "Todo:     " (String.fromInt todoCount)
    in
    [ headline
    , stat "Duration: " (String.fromInt (round totalDuration) ++ " ms")
    , stat "Passed:   " (String.fromInt passedCount)
    , stat "Failed:   " (String.fromInt failedCount)
    , todoStats
    ]
        |> Text.concat
        |> Text.render useColor


stat : String -> String -> Text
stat label value =
    Text.concat
        [ dark (plain label)
        , plain (value ++ "\n")
        ]


withChar : Char -> String -> String
withChar icon str =
    String.fromChar icon ++ " " ++ str ++ "\n"
