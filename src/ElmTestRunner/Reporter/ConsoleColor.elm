module ElmTestRunner.Reporter.ConsoleColor exposing (implementation)

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
import ElmTestRunner.Vendor.ConsoleText as Text exposing (Text, UseColor, dark, green, plain, red, underline, yellow)
import ElmTestRunner.Vendor.FormatColor as FormatColor
import ElmTestRunner.Vendor.FormatMonochrome as FormatMonochrome
import Test.Coverage exposing (CoverageReport)
import Test.Runner exposing (formatLabels)


{-| Provide a console implementation of a reporter, mostly for human consumption.
Require the initial random seed and number of fuzz runs.
-}
implementation : UseColor -> { seed : Int, fuzzRuns : Int, verbosity : Int } -> Interface
implementation useColor options =
    { onBegin = onBegin options >> Maybe.map (Text.render useColor)
    , onResult = onResult useColor >> Maybe.map (Text.render useColor)
    , onEnd = \kindResult testResults -> Maybe.map (Text.render useColor) (onEnd options.verbosity kindResult testResults)
    }


{-| Text output when starting the test runners.
-}
onBegin : { seed : Int, fuzzRuns : Int, verbosity : Int } -> Int -> Maybe Text
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
        Passed { labels, successes } ->
            successes
                -- TODO show labels as well
                |> List.filterMap coverageReportToString
                |> String.join "\n\n\n"
                |> plain
                |> Just

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


coverageReportToString : CoverageReport -> Maybe String
coverageReportToString coverageReport =
    case coverageReport of
        Test.Coverage.NoCoverage ->
            Nothing

        Test.Coverage.CoverageToReport r ->
            Just (Test.Coverage.coverageReportTable r)

        Test.Coverage.CoverageCheckSucceeded _ ->
            {- Not reporting the table to the Console.Color stdout (similarly to
               the Console reporter) although the data is technically there.
               We keep the full data dump for the JSON reporter.
            -}
            Nothing

        Test.Coverage.CoverageCheckFailed _ ->
            -- The table is included in the failure message already.
            Nothing


failureToText : UseColor -> ( Failure, CoverageReport ) -> Text
failureToText useColor ( { given, description, reason }, coverageReport ) =
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
    [ coverageReport
        |> coverageReportToString
        |> Maybe.map (\str -> dark (plain str))
    , given |> Maybe.map (\str -> dark (plain ("\nGiven " ++ str ++ "\n")))
    , Just messageText
    ]
        |> List.filterMap identity
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
onEnd : Int -> Result String Kind -> Array TestResult -> Maybe Text
onEnd verbosityLevel kindResult testResults =
    case kindResult of
        Err err ->
            plain ("Your tests are invalid: " ++ err ++ "\n")
                |> Just

        Ok kind ->
            if Array.isEmpty testResults then
                Nothing

            else if verbosityLevel <= 0 then
                Just (formatSummary kind (TestResult.summary testResults))

            else
                Just
                    (Text.concat
                        [ Text.plain "Tests listing:\n\n"
                        , formatTestListing testResults
                        , formatSummary kind (TestResult.summary testResults)
                        ]
                    )



-- Format the test listing #####################################################


{-| Data structure to gather all tests results in a tree,
to be able to report visually pleasing listing of all tests.
-}
type TestListingTree
    = TestListingTree
        { tests : List { passed : Bool, label : String }
        , groups : Dict String TestListingTree
        }


type Either a b
    = Left a
    | Right b


{-| Format a listing of all tests, assuming their order makes some sense, so keeping it.
-}
formatTestListing : Array TestResult -> Text
formatTestListing testResults =
    let
        accumListingLine : Int -> Either { passed : Bool, label : String } String -> List Text -> List Text
        accumListingLine indentationLevel testLine accumText =
            formatListingLine indentationLevel testLine :: accumText
    in
    listingTraversal accumListingLine 0 (toListingTree testResults) []
        |> List.reverse
        |> Text.concat


{-| Apply an accumulation function while traversing the test listing.
The traversal starts by all tests of a given level, followed by groups of that level.
While traversing a group, it first encounters the group name, followed by its subtree of tests.
-}
listingTraversal : (Int -> Either { passed : Bool, label : String } String -> acc -> acc) -> Int -> TestListingTree -> acc -> acc
listingTraversal f initialLevel (TestListingTree { tests, groups }) acc =
    let
        oneGroupTraversal : ( String, TestListingTree ) -> acc -> acc
        oneGroupTraversal ( group, subTree ) subAcc =
            f initialLevel (Right group) subAcc
                |> listingTraversal f (initialLevel + 1) subTree

        groupsTraversal : acc -> acc
        groupsTraversal groupsAcc =
            List.foldl oneGroupTraversal groupsAcc (Dict.toList groups)
    in
    -- Tests traversal
    List.foldl (\test -> f initialLevel (Left test)) acc tests
        -- Then groups traversal
        |> groupsTraversal


{-| Format one line in the test listing.
That line may be a test result line or the name of a group of tests.
-}
formatListingLine : Int -> Either { passed : Bool, label : String } String -> Text
formatListingLine indentationLevel testLine =
    case testLine of
        Left { passed, label } ->
            if passed then
                Text.plain (String.concat [ String.repeat indentationLevel "  ", "✓ PASSED: ", label, "\n" ])

            else
                Text.red (String.concat [ String.repeat indentationLevel "  ", "✗ FAILED: ", label, "\n" ])

        Right groupName ->
            Text.plain (String.concat [ String.repeat indentationLevel "  ", "↓ ", groupName, "\n" ])


{-| Convert the array of test results into a listing tree.
-}
toListingTree : Array TestResult -> TestListingTree
toListingTree testResults =
    Array.foldr (statusAndLabels >> insertTestInListing) emptyListingTree testResults


statusAndLabels : TestResult -> { passed : Bool, labels : List String }
statusAndLabels testResult =
    case testResult of
        TestResult.Failed { labels } ->
            { passed = False, labels = List.reverse labels }

        TestResult.Passed { labels } ->
            { passed = True, labels = List.reverse labels }


insertTestInListing : { passed : Bool, labels : List String } -> TestListingTree -> TestListingTree
insertTestInListing { passed, labels } (TestListingTree { tests, groups }) =
    case labels of
        -- Should not happen
        [] ->
            emptyListingTree

        label :: [] ->
            TestListingTree
                { tests = { passed = passed, label = label } :: tests
                , groups = groups
                }

        groupLabel :: subLabels ->
            TestListingTree
                { tests = tests
                , groups =
                    let
                        subTree =
                            Dict.get groupLabel groups
                                |> Maybe.withDefault emptyListingTree
                    in
                    Dict.insert groupLabel (insertTestInListing { passed = passed, labels = subLabels } subTree) groups
                }


emptyListingTree : TestListingTree
emptyListingTree =
    TestListingTree { tests = [], groups = Dict.empty }



-- Format the summary ##########################################################


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
