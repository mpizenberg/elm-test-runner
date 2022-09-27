module ElmTestRunner.Reporter.Json exposing (implementation)

{-| Json implementation of a reporter

@docs implementation

-}

import Array exposing (Array)
import Dict exposing (Dict)
import ElmTestRunner.Failure exposing (Failure)
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (TestResult(..))
import ElmTestRunner.SeededRunners exposing (Kind(..))
import Json.Encode as Encode
import Test.Coverage exposing (CoverageReport(..))
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
    Just <|
        (Encode.encode 0 <|
            Encode.object
                [ ( "event", Encode.string "runStart" )
                , ( "testCount", Encode.string (String.fromInt testsCount) )
                , ( "initialSeed", Encode.string (String.fromInt seed) )
                , ( "fuzzRuns", Encode.string (String.fromInt fuzzRuns) )
                , ( "globs", Encode.list Encode.string globs )
                , ( "paths", Encode.list Encode.string paths )
                ]
        )
            ++ "\n"


onResult : TestResult -> Maybe String
onResult result =
    let
        { status, testLabels, testFailures, testCoverageReports, testDuration } =
            case result of
                Passed { labels, duration, coverageReports } ->
                    { status = "pass"
                    , testLabels = List.reverse labels
                    , testFailures = Encode.list Encode.string []
                    , testCoverageReports = Encode.list encodeCoverageReport coverageReports
                    , testDuration = duration
                    }

                Failed { labels, duration, todos, failures, coverageReports } ->
                    if not (List.isEmpty todos) then
                        { status = "todo"
                        , testLabels = List.reverse labels
                        , testFailures = Encode.list Encode.string todos
                        , testCoverageReports = Encode.list identity []
                        , testDuration = duration
                        }

                    else
                        { status = "fail"
                        , testLabels = List.reverse labels
                        , testFailures = Encode.list encodeFailure failures
                        , testCoverageReports = Encode.list encodeCoverageReport coverageReports
                        , testDuration = duration
                        }
    in
    Just <|
        (Encode.encode 0 <|
            Encode.object
                [ ( "event", Encode.string "testCompleted" )
                , ( "status", Encode.string status )
                , ( "labels", Encode.list Encode.string testLabels )
                , ( "failures", testFailures )
                , ( "coverageReports", testCoverageReports )
                , ( "duration", Encode.string (String.fromFloat testDuration) )
                ]
        )
            ++ "\n"


onEnd : Result String Kind -> Array TestResult -> Maybe String
onEnd kind results =
    let
        { totalDuration, passedCount, failedCount } =
            TestResult.summary results

        autofail =
            case kind of
                Ok Plain ->
                    Encode.null

                Ok Only ->
                    Encode.string "Test.only was used"

                Ok Skipping ->
                    Encode.string "Test.skip was used"

                Err err ->
                    Encode.string err
    in
    Just <|
        (Encode.encode 0 <|
            Encode.object
                [ ( "event", Encode.string "runComplete" )
                , ( "passed", Encode.string (String.fromInt passedCount) )
                , ( "failed", Encode.string (String.fromInt failedCount) )
                , ( "duration", Encode.string (String.fromFloat totalDuration) )
                , ( "autoFail", autofail )
                ]
        )
            ++ "\n"


encodeFailure : Failure -> Encode.Value
encodeFailure { given, description, reason } =
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


encodeCoverageReport : CoverageReport -> Encode.Value
encodeCoverageReport coverageReport =
    case coverageReport of
        NoCoverage ->
            Encode.null
                |> encodeSumType "NoCoverage"

        CoverageToReport r ->
            [ ( "coverageCount", encodeCoverageCount r.coverageCount )
            , ( "runsElapsed", Encode.int r.runsElapsed )
            ]
                |> Encode.object
                |> encodeSumType "CoverageToReport"

        CoverageCheckSucceeded r ->
            [ ( "coverageCount", encodeCoverageCount r.coverageCount )
            , ( "runsElapsed", Encode.int r.runsElapsed )
            ]
                |> Encode.object
                |> encodeSumType "CoverageCheckSucceeded"

        CoverageCheckFailed r ->
            [ ( "coverageCount", encodeCoverageCount r.coverageCount )
            , ( "runsElapsed", Encode.int r.runsElapsed )
            , ( "badLabel", Encode.string r.badLabel )
            , ( "badLabelPercentage", Encode.float r.badLabelPercentage )
            , ( "expectedCoverage", Encode.string r.expectedCoverage )
            ]
                |> Encode.object
                |> encodeSumType "CoverageCheckFailed"


encodeSumType : String -> Encode.Value -> Encode.Value
encodeSumType sumType data =
    Encode.object
        [ ( "type", Encode.string sumType )
        , ( "data", data )
        ]


encodeCoverageCount : Dict (List String) Int -> Encode.Value
encodeCoverageCount dict =
    dict
        |> Dict.toList
        |> Encode.list
            (\( labels, count ) ->
                Encode.object
                    [ ( "labels", Encode.list Encode.string labels )
                    , ( "count", Encode.int count )
                    ]
            )
