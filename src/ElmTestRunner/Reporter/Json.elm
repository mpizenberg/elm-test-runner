module ElmTestRunner.Reporter.Json exposing (implementation)

{-| Json implementation of a reporter

@docs implementation

-}

import Array exposing (Array)
import ElmTestRunner.Failure exposing (Failure)
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (TestResult(..))
import ElmTestRunner.SeededRunners exposing (Kind(..))
import Json.Encode as Encode
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| Provide a Json implementation of a reporter, mostly for automated tools.
Require the initial random seed and number of fuzz runs.
-}
implementation : { seed : Int, fuzzRuns : Int, globs : List String, paths : List String } -> Interface
implementation options =
    { onBegin = onBegin options
    , onResult = onResult
    , onEnd = onEnd
    }


onBegin : { seed : Int, fuzzRuns : Int, globs : List String, paths : List String } -> Int -> Maybe String
onBegin { seed, fuzzRuns, globs, paths } testsCount =
    """{"event":"runStart","testCount":"{{ testsCount }}","initialSeed":"{{ seed }}","fuzzRuns":"{{ fuzzRuns }}","globs":{{ globs }},"paths":{{ paths }}}
"""
        |> String.replace "{{ testsCount }}" (String.fromInt testsCount)
        |> String.replace "{{ seed }}" (String.fromInt seed)
        |> String.replace "{{ fuzzRuns }}" (String.fromInt fuzzRuns)
        |> String.replace "{{ globs }}" (Encode.encode 0 <| Encode.list Encode.string globs)
        |> String.replace "{{ paths }}" (Encode.encode 0 <| Encode.list Encode.string paths)
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
                        , testFailures = Encode.encode 0 (Encode.list jsonEncodeFailure failures)
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

        autofail =
            case kind of
                Ok Plain ->
                    "null"

                Ok Only ->
                    "\"Test.only was used\""

                Ok Skipping ->
                    "\"Test.skip was used\""

                Err err ->
                    "\"" ++ err ++ "\""
    in
    """{"event":"runComplete","passed":"{{ passed }}","failed":"{{ failed }}","duration":"{{ duration }}","autoFail":{{ autofail }}}
"""
        |> String.replace "{{ passed }}" (String.fromInt passedCount)
        |> String.replace "{{ failed }}" (String.fromInt failedCount)
        |> String.replace "{{ duration }}" (String.fromFloat totalDuration)
        |> String.replace "{{ autofail }}" autofail
        |> Just


jsonEncodeFailure : Failure -> Encode.Value
jsonEncodeFailure { given, description, reason } =
    Encode.object
        [ ( "given", Maybe.withDefault Encode.null (Maybe.map Encode.string given) )
        , ( "message", Encode.string description )
        , ( "reason", encodeReason description reason )
        ]


encodeReasonType : String -> Encode.Value -> Encode.Value
encodeReasonType reasonType data =
    Encode.object
        [ ( "type", Encode.string reasonType ), ( "data", data ) ]


encodeReason : String -> Reason -> Encode.Value
encodeReason description reason =
    case reason of
        Custom ->
            Encode.string description
                |> encodeReasonType "Custom"

        Equality expected actual ->
            [ ( "expected", Encode.string expected )
            , ( "actual", Encode.string actual )
            , ( "comparison", Encode.string description )
            ]
                |> Encode.object
                |> encodeReasonType "Equality"

        Comparison first second ->
            [ ( "first", Encode.string first )
            , ( "second", Encode.string second )
            , ( "comparison", Encode.string description )
            ]
                |> Encode.object
                |> encodeReasonType "Comparison"

        TODO ->
            Encode.string description
                |> encodeReasonType "TODO"

        Invalid BadDescription ->
            let
                explanation =
                    if description == "" then
                        "The empty string is not a valid test description."

                    else
                        "This is an invalid test description: " ++ description
            in
            Encode.string explanation
                |> encodeReasonType "Invalid"

        Invalid _ ->
            Encode.string description
                |> encodeReasonType "Invalid"

        ListDiff expected actual ->
            [ ( "expected", Encode.list Encode.string expected )
            , ( "actual", Encode.list Encode.string actual )
            ]
                |> Encode.object
                |> encodeReasonType "ListDiff"

        CollectionDiff { expected, actual, extra, missing } ->
            [ ( "expected", Encode.string expected )
            , ( "actual", Encode.string actual )
            , ( "extra", Encode.list Encode.string extra )
            , ( "missing", Encode.list Encode.string missing )
            ]
                |> Encode.object
                |> encodeReasonType "CollectionDiff"
