module ElmTestRunner.Reporter.Console exposing (implementation)

{-| Console implementation of a reporter

@docs implementation

-}

import Array exposing (Array)
import ElmTestRunner.Failure as Failure exposing (Failure)
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (Summary, TestResult(..))
import ElmTestRunner.SeededRunners exposing (Kind(..))


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


onResult : TestResult -> Maybe String
onResult result =
    case result of
        Passed _ ->
            Nothing

        Failed { labels, todos, failures, logs } ->
            Just <|
                String.join "\n"
                    [ ""
                    , formatLabels labels
                    , ""
                    , indent (displayFailureContent todos failures logs)
                    ]


formatLabels : List String -> String
formatLabels =
    formatLabelsHelp []


formatLabelsHelp : List String -> List String -> String
formatLabelsHelp formattedLines labels =
    case ( formattedLines, labels ) of
        ( _, [] ) ->
            String.join "\n" formattedLines

        -- First is the test name
        ( [], testName :: location ) ->
            formatLabelsHelp [ "X " ++ testName ] location

        ( _, loc :: location ) ->
            formatLabelsHelp (("| " ++ loc) :: formattedLines) location


indent : String -> String
indent str =
    String.split "\n" str
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"


displayFailureContent : List String -> List Failure -> List String -> String
displayFailureContent todos failures logs =
    "with todos: {{ todos }}\nwith failures: {{ failures }}\nwith debug logs:\n\n{{ logs }}\n"
        |> String.replace "{{ todos }}" (Debug.toString todos)
        |> String.replace "{{ failures }}" (Debug.toString failures)
        |> String.replace "{{ logs }}" (String.concat logs)


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
    """
TEST RUN {{ result }}

Passed:   {{ passed }}
Failed:   {{ failed }}
Running duration (workers): {{ duration }} ms
"""
        |> String.replace "{{ result }}" (summaryTitle kind failedCount todoCount)
        |> String.replace "{{ duration }}" (String.fromInt (round totalDuration))
        |> String.replace "{{ passed }}" (String.fromInt passedCount)
        |> String.replace "{{ failed }}" (String.fromInt failedCount)


summaryTitle : Kind -> Int -> Int -> String
summaryTitle kind failedCount todoCount =
    case ( kind, failedCount, todoCount ) of
        ( Plain, 0, 0 ) ->
            "PASSED"

        ( Plain, 0, 1 ) ->
            "INCOMPLETE (because there is a Test.todo left)"

        ( Plain, 0, _ ) ->
            "INCOMPLETE (because there are some Test.todo left)"

        ( Only, 0, _ ) ->
            "INCOMPLETE (because Test.only was used)"

        ( Skipping, 0, _ ) ->
            "INCOMPLETE (because at least one test is skipped)"

        _ ->
            "FAILED"
