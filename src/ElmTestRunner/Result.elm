module ElmTestRunner.Result exposing
    ( TestResult(..), fromExpectations, setDuration, setLogs, encode, decoder
    , Summary, summary
    )

{-| Types and functions to manipulate a test result.


# Manipulation of the result of a test run

@docs TestResult, fromExpectations, setDuration, setLogs, encode, decoder


# Helper functions

@docs Summary, summary

-}

import Array exposing (Array)
import Dict exposing (Dict)
import ElmTestRunner.Failure as Failure exposing (Failure)
import Expect exposing (Expectation)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Test.Coverage exposing (CoverageReport(..))
import Test.Runner


{-| Type summarizing the results of a test run.
It is obtained from the list of expectations returned by calling runner.run ().
-}
type TestResult
    = Passed
        { labels : List String
        , duration : Float
        , logs : List String
        , coverageReports : List CoverageReport
        }
    | Failed
        { labels : List String
        , duration : Float
        , logs : List String
        , todos : List String
        , failures : List Failure
        , coverageReports : List CoverageReport
        }


{-| Set the duration that the test took.
-}
setDuration : Float -> TestResult -> TestResult
setDuration duration testResult =
    case testResult of
        Passed { labels, logs, coverageReports } ->
            Passed
                { labels = labels
                , duration = duration
                , logs = logs
                , coverageReports = coverageReports
                }

        Failed { labels, logs, todos, failures, coverageReports } ->
            Failed
                { labels = labels
                , duration = duration
                , logs = logs
                , todos = todos
                , failures = failures
                , coverageReports = coverageReports
                }


{-| Set the logs received for that test.
-}
setLogs : List String -> TestResult -> TestResult
setLogs logs testResult =
    case testResult of
        Passed { labels, duration, coverageReports } ->
            Passed
                { labels = labels
                , duration = duration
                , logs = logs
                , coverageReports = coverageReports
                }

        Failed { labels, duration, todos, failures, coverageReports } ->
            Failed
                { labels = labels
                , duration = duration
                , logs = logs
                , todos = todos
                , failures = failures
                , coverageReports = coverageReports
                }


{-| Convert a list of expectations (results of a run) into a `TestResult`.
Return the `Failed` variant if there is any todo or failure in the expectations.
-}
fromExpectations : List String -> List Expectation -> TestResult
fromExpectations labels expectations =
    let
        outcomes : Outcomes
        outcomes =
            getOutcomes expectations
    in
    if List.isEmpty outcomes.todos && List.isEmpty outcomes.failures then
        Passed
            { labels = labels
            , duration = 0
            , logs = []
            , coverageReports = outcomes.coverageReports
            }

    else
        Failed
            { labels = labels
            , duration = 0
            , logs = []
            , todos = outcomes.todos
            , failures = outcomes.failures
            , coverageReports = outcomes.coverageReports
            }


type alias Outcomes =
    { todos : List String
    , failures : List Failure
    , coverageReports : List CoverageReport
    }


initOutcomes : Outcomes
initOutcomes =
    { todos = []
    , failures = []
    , coverageReports = []
    }


getOutcomes : List Expectation -> Outcomes
getOutcomes expectations =
    List.foldl accumOutcomes initOutcomes expectations


accumOutcomes : Expectation -> Outcomes -> Outcomes
accumOutcomes expectation outcomes =
    let
        coverageReport : CoverageReport
        coverageReport =
            Test.Runner.getCoverageReport expectation
    in
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            { outcomes | coverageReports = coverageReport :: outcomes.coverageReports }

        Just failure ->
            if Test.Runner.isTodo expectation then
                { outcomes | todos = failure.description :: outcomes.todos }

            else
                { outcomes
                    | failures = failure :: outcomes.failures
                    , coverageReports = coverageReport :: outcomes.coverageReports
                }


{-| Encode a `TestResult`.
-}
encode : TestResult -> Value
encode =
    encodeTestResult


{-| Decode a `TestResult`.
-}
decoder : Decoder TestResult
decoder =
    decodeTestResult


{-| Quantitative summary of all test results.
-}
type alias Summary =
    { totalDuration : Float, passedCount : Int, failedCount : Int, todoCount : Int }


{-| Report a quantitative summary of test results.
-}
summary : Array TestResult -> Summary
summary =
    Array.foldl accumStats { totalDuration = 0, passedCount = 0, failedCount = 0, todoCount = 0 }


accumStats : TestResult -> Summary -> Summary
accumStats result { totalDuration, passedCount, failedCount, todoCount } =
    case result of
        Passed { duration } ->
            { totalDuration = totalDuration + duration
            , passedCount = passedCount + 1
            , failedCount = failedCount
            , todoCount = todoCount
            }

        Failed { duration, todos, failures } ->
            let
                newFailedCount =
                    if List.isEmpty failures then
                        failedCount

                    else
                        failedCount + 1

                newTodoCount =
                    if List.isEmpty todos then
                        todoCount

                    else
                        todoCount + 1
            in
            { totalDuration = totalDuration + duration
            , passedCount = passedCount
            , failedCount = newFailedCount
            , todoCount = newTodoCount
            }



-- Functions needed by the automatically generated decoders


decodeFailure =
    Failure.decoder


encodeFailure =
    Failure.encode



-- Automatically generated decoders and encoders for TestResult with https://dkodaj.github.io/decgen/


type alias Record_coverageCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedCoverage_String_ =
    { coverageCount : Dict (List String) Int, runsElapsed : Int, badLabel : String, badLabelPercentage : Float, expectedCoverage : String }


type alias Record_coverageCount_Dict_ListString_Int_runsElapsed_Int_ =
    { coverageCount : Dict (List String) Int, runsElapsed : Int }


type alias Record_expected_String_actual_String_extra_ListString_missing_ListString_ =
    { expected : String, actual : String, extra : List String, missing : List String }


type alias Record_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_coverageReports_ListCoverageReport_ =
    { labels : List String, duration : Float, logs : List String, todos : List String, failures : List Failure, coverageReports : List CoverageReport }


type alias Record_labels_ListString_duration_Float_logs_ListString_coverageReports_ListCoverageReport_ =
    { labels : List String, duration : Float, logs : List String, coverageReports : List CoverageReport }


decodeCoverageReport : Decoder CoverageReport
decodeCoverageReport =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeCoverageReportHelp


decodeCoverageReportHelp constructor =
    case constructor of
        "NoCoverage" ->
            Decode.succeed NoCoverage

        "CoverageToReport" ->
            Decode.map
                CoverageToReport
                (Decode.field "A1" decodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_)

        "CoverageCheckSucceeded" ->
            Decode.map
                CoverageCheckSucceeded
                (Decode.field "A1" decodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_)

        "CoverageCheckFailed" ->
            Decode.map
                CoverageCheckFailed
                (Decode.field "A1" decodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedCoverage_String_)

        other ->
            Decode.fail <| "Unknown constructor for type CoverageReport: " ++ other


decodeDict_ListString_Int : Decoder (Dict (List String) Int)
decodeDict_ListString_Int =
    let
        decodeDict_ListString_IntTuple =
            Decode.map2
                (\a1 a2 -> ( a1, a2 ))
                (Decode.field "A1" (Decode.list Decode.string))
                (Decode.field "A2" Decode.int)
    in
    Decode.map Dict.fromList (Decode.list decodeDict_ListString_IntTuple)


decodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_ =
    Decode.map2
        Record_coverageCount_Dict_ListString_Int_runsElapsed_Int_
        (Decode.field "coverageCount" decodeDict_ListString_Int)
        (Decode.field "runsElapsed" Decode.int)


decodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedCoverage_String_ =
    Decode.map5
        Record_coverageCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedCoverage_String_
        (Decode.field "coverageCount" decodeDict_ListString_Int)
        (Decode.field "runsElapsed" Decode.int)
        (Decode.field "badLabel" Decode.string)
        (Decode.field "badLabelPercentage" Decode.float)
        (Decode.field "expectedCoverage" Decode.string)


decodeRecord_expected_String_actual_String_extra_ListString_missing_ListString_ =
    Decode.map4
        Record_expected_String_actual_String_extra_ListString_missing_ListString_
        (Decode.field "expected" Decode.string)
        (Decode.field "actual" Decode.string)
        (Decode.field "extra" (Decode.list Decode.string))
        (Decode.field "missing" (Decode.list Decode.string))


decodeRecord_labels_ListString_duration_Float_logs_ListString_coverageReports_ListCoverageReport_ =
    Decode.map4
        Record_labels_ListString_duration_Float_logs_ListString_coverageReports_ListCoverageReport_
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.field "duration" Decode.float)
        (Decode.field "logs" (Decode.list Decode.string))
        (Decode.field "coverageReports" (Decode.list decodeCoverageReport))


decodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_coverageReports_ListCoverageReport_ =
    Decode.map6
        Record_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_coverageReports_ListCoverageReport_
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.field "duration" Decode.float)
        (Decode.field "logs" (Decode.list Decode.string))
        (Decode.field "todos" (Decode.list Decode.string))
        (Decode.field "failures" (Decode.list decodeFailure))
        (Decode.field "coverageReports" (Decode.list decodeCoverageReport))


decodeTestResult : Decoder TestResult
decodeTestResult =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeTestResultHelp


decodeTestResultHelp constructor =
    case constructor of
        "Passed" ->
            Decode.map
                Passed
                (Decode.field "A1" decodeRecord_labels_ListString_duration_Float_logs_ListString_coverageReports_ListCoverageReport_)

        "Failed" ->
            Decode.map
                Failed
                (Decode.field "A1" decodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_coverageReports_ListCoverageReport_)

        other ->
            Decode.fail <| "Unknown constructor for type TestResult: " ++ other


encodeCoverageReport : CoverageReport -> Value
encodeCoverageReport a =
    case a of
        NoCoverage ->
            Encode.object
                [ ( "Constructor", Encode.string "NoCoverage" )
                ]

        CoverageToReport a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "CoverageToReport" )
                , ( "A1", encodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_ a1 )
                ]

        CoverageCheckSucceeded a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "CoverageCheckSucceeded" )
                , ( "A1", encodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_ a1 )
                ]

        CoverageCheckFailed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "CoverageCheckFailed" )
                , ( "A1", encodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedCoverage_String_ a1 )
                ]


encodeDict_ListString_Int : Dict (List String) Int -> Value
encodeDict_ListString_Int a =
    let
        encodeDict_ListString_IntTuple ( a1, a2 ) =
            Encode.object
                [ ( "A1", Encode.list Encode.string a1 )
                , ( "A2", Encode.int a2 )
                ]
    in
    Encode.list encodeDict_ListString_IntTuple (Dict.toList a)


encodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_ a =
    Encode.object
        [ ( "coverageCount", encodeDict_ListString_Int a.coverageCount )
        , ( "runsElapsed", Encode.int a.runsElapsed )
        ]


encodeRecord_coverageCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedCoverage_String_ a =
    Encode.object
        [ ( "coverageCount", encodeDict_ListString_Int a.coverageCount )
        , ( "runsElapsed", Encode.int a.runsElapsed )
        , ( "badLabel", Encode.string a.badLabel )
        , ( "badLabelPercentage", Encode.float a.badLabelPercentage )
        , ( "expectedCoverage", Encode.string a.expectedCoverage )
        ]


encodeRecord_expected_String_actual_String_extra_ListString_missing_ListString_ a =
    Encode.object
        [ ( "expected", Encode.string a.expected )
        , ( "actual", Encode.string a.actual )
        , ( "extra", Encode.list Encode.string a.extra )
        , ( "missing", Encode.list Encode.string a.missing )
        ]


encodeRecord_labels_ListString_duration_Float_logs_ListString_coverageReports_ListCoverageReport_ a =
    Encode.object
        [ ( "labels", Encode.list Encode.string a.labels )
        , ( "duration", Encode.float a.duration )
        , ( "logs", Encode.list Encode.string a.logs )
        , ( "coverageReports", Encode.list encodeCoverageReport a.coverageReports )
        ]


encodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_coverageReports_ListCoverageReport_ a =
    Encode.object
        [ ( "labels", Encode.list Encode.string a.labels )
        , ( "duration", Encode.float a.duration )
        , ( "logs", Encode.list Encode.string a.logs )
        , ( "todos", Encode.list Encode.string a.todos )
        , ( "failures", Encode.list encodeFailure a.failures )
        , ( "coverageReports", Encode.list encodeCoverageReport a.coverageReports )
        ]


encodeTestResult : TestResult -> Value
encodeTestResult a =
    case a of
        Passed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Passed" )
                , ( "A1", encodeRecord_labels_ListString_duration_Float_logs_ListString_coverageReports_ListCoverageReport_ a1 )
                ]

        Failed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Failed" )
                , ( "A1", encodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_coverageReports_ListCoverageReport_ a1 )
                ]
