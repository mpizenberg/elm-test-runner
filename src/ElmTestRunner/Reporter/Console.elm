module ElmTestRunner.Reporter.Console exposing (implementation)

{-| Console implementation of a reporter

@docs implementation

-}

import Array exposing (Array)
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (Summary, TestResult(..))


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
            """
{{ labels }}

    with todos: {{ todos }}
    with failures: {{ failures }}
    with debug logs:

{{ logs }}

"""
                |> String.replace "{{ labels }}" (formatLabels labels)
                |> String.replace "{{ todos }}" (Debug.toString todos)
                |> String.replace "{{ failures }}" (Debug.toString failures)
                |> String.replace "{{ logs }}" (String.concat logs)
                |> Just


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


onEnd : Array TestResult -> Maybe String
onEnd testResults =
    formatSummary (TestResult.summary testResults)
        |> Just


formatSummary : Summary -> String
formatSummary { totalDuration, passedCount, failedCount } =
    """
TEST RUN {{ result }}

Passed:   {{ passed }}
Failed:   {{ failed }}
Running duration (workers): {{ duration }} ms
"""
        |> String.replace "{{ result }}" (summaryTitle (failedCount > 0))
        |> String.replace "{{ duration }}" (String.fromInt (round totalDuration))
        |> String.replace "{{ passed }}" (String.fromInt passedCount)
        |> String.replace "{{ failed }}" (String.fromInt failedCount)


summaryTitle : Bool -> String
summaryTitle failed =
    if failed then
        "FAILED"

    else
        "PASSED"
