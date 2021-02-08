module ElmTestRunner.Reporter.Json exposing (implementation)

{-| Json implementation of a reporter

@docs implementation

-}

import Array exposing (Array)
import ElmTestRunner.Failure as Failure
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (TestResult(..))
import ElmTestRunner.SeededRunners exposing (Kind(..))
import Json.Encode as Encode


{-| Provide a Json implementation of a reporter, mostly for automated tools.
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
    """{"event":"runStart","testCount":"{{ testsCount }}","initialSeed":"{{ seed }}","fuzzRuns":"{{ fuzzRuns }}","globs":[{{ globs }}],"paths":[{{ paths }}]}
"""
        |> String.replace "{{ testsCount }}" (String.fromInt testsCount)
        |> String.replace "{{ seed }}" (String.fromInt seed)
        |> String.replace "{{ fuzzRuns }}" (String.fromInt fuzzRuns)
        |> String.replace "{{ globs }}" ""
        |> String.replace "{{ paths }}" ""
        |> Just


onResult : TestResult -> Maybe String
onResult result =
    let
        { status, testLabels, testFailures, testDuration } =
            case result of
                Passed { labels, duration } ->
                    { status = "pass"
                    , testLabels = Encode.encode 0 (Encode.list Encode.string (List.reverse labels))
                    , testFailures = "[]"
                    , testDuration = duration
                    }

                Failed { labels, duration, todos, failures } ->
                    if not (List.isEmpty todos) then
                        { status = "todo"
                        , testLabels = Encode.encode 0 (Encode.list Encode.string (List.reverse labels))
                        , testFailures = Encode.encode 0 (Encode.list Encode.string todos)
                        , testDuration = duration
                        }

                    else
                        { status = "fail"
                        , testLabels = Encode.encode 0 (Encode.list Encode.string (List.reverse labels))
                        , testFailures = Encode.encode 0 (Encode.list Failure.encode failures)
                        , testDuration = duration
                        }
    in
    """{"event":"testCompleted","status":"{{ status }}","labels":{{ labels }},"failures":{{ failures }},"duration":"{{ duration }}"}
"""
        |> String.replace "{{ status }}" status
        |> String.replace "{{ labels }}" testLabels
        |> String.replace "{{ failures }}" testFailures
        |> String.replace "{{ duration }}" (String.fromFloat testDuration)
        |> Just


onEnd : Result String Kind -> Array TestResult -> Maybe String
onEnd kind results =
    let
        { totalDuration, passedCount, failedCount } =
            TestResult.summary results
    in
    """{"event":"runComplete","passed":"{{ passed }}","failed":"{{ failed }}","duration":"{{ duration }}","autoFail":null}
"""
        |> String.replace "{{ passed }}" (String.fromInt passedCount)
        |> String.replace "{{ failed }}" (String.fromInt failedCount)
        |> String.replace "{{ duration }}" (String.fromFloat totalDuration)
        |> Just
