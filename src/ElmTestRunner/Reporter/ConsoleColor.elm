module ElmTestRunner.Reporter.ConsoleColor exposing (implementation)

{-| Console reporter with color support

@docs implementation

-}

import Array exposing (Array)
import ElmTestRunner.Failure exposing (Failure)
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (Summary, TestResult(..))
import ElmTestRunner.SeededRunners exposing (Kind(..))
import ElmTestRunner.Vendor.ConsoleFormat exposing (format)
import ElmTestRunner.Vendor.ConsoleText as Text exposing (Text, UseColor, dark, green, plain, red, underline, yellow)
import ElmTestRunner.Vendor.FormatColor as FormatColor
import ElmTestRunner.Vendor.FormatMonochrome as FormatMonochrome
import Test.Runner exposing (formatLabels)


{-| Provide a console implementation of a reporter, mostly for human consumption.
Require the initial random seed and number of fuzz runs.
-}
implementation : UseColor -> { seed : Int, fuzzRuns : Int } -> Interface
implementation useColor options =
    { onBegin = onBegin options >> Maybe.map (Text.render useColor)
    , onResult = onResult useColor >> Maybe.map (Text.render useColor)
    , onEnd = \kindResult testResults -> Maybe.map (Text.render useColor) (onEnd kindResult testResults)
    }


{-| Text output when starting the test runners.
-}
onBegin : { seed : Int, fuzzRuns : Int } -> Int -> Maybe Text
onBegin { seed, fuzzRuns } testsCount =
    """
Running {{ testsCount }} tests. To reproduce these results later,
run elm-test-rs with --seed {{ seed }} and --fuzz {{ fuzzRuns }}

"""
        |> String.replace "{{ testsCount }}" (String.fromInt testsCount)
        |> String.replace "{{ seed }}" (String.fromInt seed)
        |> String.replace "{{ fuzzRuns }}" (String.fromInt fuzzRuns)
        |> plain
        |> Just



--


{-| Text output when receiving a test result.
-}
onResult : UseColor -> TestResult -> Maybe Text
onResult useColor testResult =
    case testResult of
        Passed _ ->
            Nothing

        Failed { labels, todos, failures, logs } ->
            if List.isEmpty todos then
                -- We have non-TODOs still failing; report them, not the TODOs.
                List.concat
                    [ [ failureLabelsToText labels ]
                    , List.map (failureToText useColor) failures
                    , [ logsToText logs ]
                    ]
                    |> Text.concat
                    |> Just

            else
                List.map (\todo -> formatTodo labels todo ++ "\n") todos
                    |> String.concat
                    |> plain
                    |> Just


formatTodo : List String -> String -> String
formatTodo labels todo =
    String.concat
        [ "◦ "
        , String.concat (List.map (\l -> l ++ " → ") labels)
        , "TODO: "
        , todo
        ]


failureLabelsToText : List String -> Text
failureLabelsToText =
    formatLabels (dark << plain << withChar '↓') (red << withChar '✗') >> Text.concat


failureToText : UseColor -> Failure -> Text
failureToText useColor { given, description, reason } =
    let
        formatEquality =
            case useColor of
                Text.Monochrome ->
                    FormatMonochrome.formatEquality

                Text.UseColor ->
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


logsToText : List String -> Text
logsToText logs =
    if List.isEmpty logs then
        Text.plain ""

    else
        Text.plain <|
            String.join "\n"
                [ indent "with debug logs:\n"
                , String.concat logs
                , ""
                ]



--


{-| Text output when finishing running the tests.
-}
onEnd : Result String Kind -> Array TestResult -> Maybe Text
onEnd kindResult testResults =
    case kindResult of
        Err err ->
            plain ("Your tests are invalid: " ++ err ++ "\n")
                |> Just

        Ok kind ->
            formatSummary kind (TestResult.summary testResults)
                |> Just


formatSummary : Kind -> Summary -> Text
formatSummary kind { totalDuration, passedCount, failedCount, todoCount } =
    let
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


stat : String -> String -> Text
stat label value =
    Text.concat
        [ dark (plain label)
        , plain (value ++ "\n")
        ]


withChar : Char -> String -> String
withChar icon str =
    String.fromChar icon ++ " " ++ str ++ "\n"
