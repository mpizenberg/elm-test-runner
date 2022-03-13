module ElmTestRunner.Reporter.ConsoleWithNames exposing (implementation)

{-| Console reporter with color support

@docs implementation

-}

import Array exposing (Array)
import Dict exposing (Dict)
import ElmTestRunner.Failure exposing (Failure)
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (Summary, TestResult(..))
import ElmTestRunner.SeededRunners exposing (Kind(..))
import ElmTestRunner.Vendor.ConsoleFormat exposing (format)
import ElmTestRunner.Vendor.ConsoleText as Text exposing (Text, UseColor, concat, dark, green, plain, red, underline, yellow)
import ElmTestRunner.Vendor.FormatColor as FormatColor
import ElmTestRunner.Vendor.FormatMonochrome as FormatMonochrome
import Test.Runner exposing (formatLabels)


{-| Provide a console implementation of a reporter, mostly for human consumption.
Require the initial random seed and number of fuzz runs.
-}
implementation : UseColor -> { seed : Int, fuzzRuns : Int } -> Interface
implementation useColor options =
    { onBegin = onBegin options >> Maybe.map (Text.render useColor)
    , onResult = \_ -> Nothing
    , onEnd = \kindResult testResults -> Maybe.map (Text.render useColor) (onEnd useColor kindResult testResults)
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


displayFailure : UseColor -> List String -> List String -> List Failure -> List String -> Text
displayFailure useColor labels todos failures logs =
    if List.isEmpty todos then
        -- We have non-TODOs still failing; report them, not the TODOs.
        List.concat
            [ List.map (failureToText useColor) failures
            , [ logsToText logs ]
            ]
            |> Text.concat

    else
        List.map (\todo -> formatTodo labels todo ++ "\n") todos
            |> String.concat
            |> plain


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
onEnd : UseColor -> Result String Kind -> Array TestResult -> Maybe Text
onEnd useColor kindResult testResults =
    case kindResult of
        Err err ->
            plain ("Your tests are invalid: " ++ err ++ "\n")
                |> Just

        Ok kind ->
            if Array.isEmpty testResults then
                Nothing

            else
                formatSummary kind (TestResult.summary testResults)
                    |> detailedReport useColor testResults
                    |> Just


detailedReport : UseColor -> Array TestResult -> Text -> Text
detailedReport useColor testResults summary =
    [ testResults
        |> displayResults useColor
    , plain "\n\n"
    , summary
    ]
        |> concat


displayResults : UseColor -> Array TestResult -> Text
displayResults useColor testResults =
    testResults
        |> Array.map toLabels
        |> Array.foldl toDict Dict.empty
        |> viewDictItems useColor 0
        |> List.intersperse (plain "\n")
        |> concat


type TestStatus
    = TestOK
    | TestNOK { labels : List String, logs : List String, todos : List String, failures : List Failure, duration : Float }


type DictItem
    = Status TestStatus
    | D (Dict String DictItem)


toLabels : TestResult -> ( TestStatus, List String )
toLabels testResult =
    case testResult of
        Passed { labels } ->
            ( TestOK
            , labels
                |> List.reverse
            )

        Failed data ->
            ( TestNOK data
            , data.labels
                |> List.reverse
            )


toDict : ( TestStatus, List String ) -> Dict String DictItem -> Dict String DictItem
toDict ( status, labels ) dict =
    case labels of
        [] ->
            dict

        test :: [] ->
            Dict.insert test (Status status) dict

        step :: rest ->
            Dict.update step
                (\maybeD ->
                    case maybeD of
                        Nothing ->
                            Just <|
                                D <|
                                    toDict ( status, rest ) <|
                                        Dict.empty

                        Just (Status _) ->
                            Nothing

                        Just (D d) ->
                            Just <|
                                D <|
                                    toDict ( status, rest ) <|
                                        d
                )
                dict


viewDictItems : UseColor -> Int -> Dict String DictItem -> List Text
viewDictItems useColor spaces dict =
    dict
        |> Dict.foldl
            (\label d arr ->
                let
                    ( prefix, color ) =
                        case d of
                            Status TestOK ->
                                ( "✓ ", green )

                            Status (TestNOK _) ->
                                ( "✗ ", red )

                            D _ ->
                                ( "↓ ", plain )
                in
                ([ ""
                    ++ String.fromList (List.repeat spaces ' ')
                    ++ prefix
                    ++ label
                    ++ " "
                    |> color
                 , case d of
                    Status status ->
                        case status of
                            TestOK ->
                                green "→ PASSED"

                            TestNOK _ ->
                                red "→ FAILED"

                    D di ->
                        viewDictItems useColor (spaces + 2) di
                            |> (::) (plain "\n")
                            |> concat
                 , case d of
                    Status (TestNOK { labels, todos, failures, logs }) ->
                        [ plain "\n"
                        , displayFailure useColor labels todos failures logs
                        ]
                            |> concat

                    _ ->
                        plain ""
                 ]
                    |> concat
                )
                    :: arr
            )
            []
        |> List.intersperse (plain "\n")


repeatString : Int -> Char -> String
repeatString size char =
    String.fromList (List.repeat size char)


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
