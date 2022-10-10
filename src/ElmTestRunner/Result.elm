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
import Test.Distribution exposing (DistributionReport(..))
import Test.Runner


{-| Type summarizing the results of a test run.
It is obtained from the list of expectations returned by calling runner.run ().
-}
type TestResult
    = Passed
        { labels : List String
        , duration : Float
        , logs : List String
        , distributionReports : List DistributionReport
        }
    | Failed
        { labels : List String
        , duration : Float
        , logs : List String
        , todos : List String
        , failures : List Failure
        , distributionReports : List DistributionReport
        }


{-| Set the duration that the test took.
-}
setDuration : Float -> TestResult -> TestResult
setDuration duration testResult =
    case testResult of
        Passed { labels, logs, distributionReports } ->
            Passed
                { labels = labels
                , duration = duration
                , logs = logs
                , distributionReports = distributionReports
                }

        Failed { labels, logs, todos, failures, distributionReports } ->
            Failed
                { labels = labels
                , duration = duration
                , logs = logs
                , todos = todos
                , failures = failures
                , distributionReports = distributionReports
                }


{-| Set the logs received for that test.
-}
setLogs : List String -> TestResult -> TestResult
setLogs logs testResult =
    case testResult of
        Passed { labels, duration, distributionReports } ->
            Passed
                { labels = labels
                , duration = duration
                , logs = logs
                , distributionReports = distributionReports
                }

        Failed { labels, duration, todos, failures, distributionReports } ->
            Failed
                { labels = labels
                , duration = duration
                , logs = logs
                , todos = todos
                , failures = failures
                , distributionReports = distributionReports
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
            , distributionReports = outcomes.distributionReports
            }

    else
        Failed
            { labels = labels
            , duration = 0
            , logs = []
            , todos = outcomes.todos
            , failures = outcomes.failures
            , distributionReports = outcomes.distributionReports
            }


type alias Outcomes =
    { todos : List String
    , failures : List Failure
    , distributionReports : List DistributionReport
    }


initOutcomes : Outcomes
initOutcomes =
    { todos = []
    , failures = []
    , distributionReports = []
    }


getOutcomes : List Expectation -> Outcomes
getOutcomes expectations =
    List.foldl accumOutcomes initOutcomes expectations


accumOutcomes : Expectation -> Outcomes -> Outcomes
accumOutcomes expectation outcomes =
    let
        distributionReport : DistributionReport
        distributionReport =
            Test.Runner.getDistributionReport expectation
    in
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            { outcomes | distributionReports = distributionReport :: outcomes.distributionReports }

        Just failure ->
            if Test.Runner.isTodo expectation then
                { outcomes | todos = failure.description :: outcomes.todos }

            else
                { outcomes
                    | failures = failure :: outcomes.failures
                    , distributionReports = distributionReport :: outcomes.distributionReports
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


type alias Record_distributionCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedDistribution_String_ =
    { distributionCount : Dict (List String) Int, runsElapsed : Int, badLabel : String, badLabelPercentage : Float, expectedDistribution : String }


type alias Record_distributionCount_Dict_ListString_Int_runsElapsed_Int_ =
    { distributionCount : Dict (List String) Int, runsElapsed : Int }


type alias Record_expected_String_actual_String_extra_ListString_missing_ListString_ =
    { expected : String, actual : String, extra : List String, missing : List String }


type alias Record_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_distributionReports_ListDistributionReport_ =
    { labels : List String, duration : Float, logs : List String, todos : List String, failures : List Failure, distributionReports : List DistributionReport }


type alias Record_labels_ListString_duration_Float_logs_ListString_distributionReports_ListDistributionReport_ =
    { labels : List String, duration : Float, logs : List String, distributionReports : List DistributionReport }


decodeDistributionReport : Decoder DistributionReport
decodeDistributionReport =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeDistributionReportHelp


decodeDistributionReportHelp constructor =
    case constructor of
        "NoDistribution" ->
            Decode.succeed NoDistribution

        "DistributionToReport" ->
            Decode.map
                DistributionToReport
                (Decode.field "A1" decodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_)

        "DistributionCheckSucceeded" ->
            Decode.map
                DistributionCheckSucceeded
                (Decode.field "A1" decodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_)

        "DistributionCheckFailed" ->
            Decode.map
                DistributionCheckFailed
                (Decode.field "A1" decodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedDistribution_String_)

        other ->
            Decode.fail <| "Unknown constructor for type DistributionReport: " ++ other


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


decodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_ =
    Decode.map2
        Record_distributionCount_Dict_ListString_Int_runsElapsed_Int_
        (Decode.field "distributionCount" decodeDict_ListString_Int)
        (Decode.field "runsElapsed" Decode.int)


decodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedDistribution_String_ =
    Decode.map5
        Record_distributionCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedDistribution_String_
        (Decode.field "distributionCount" decodeDict_ListString_Int)
        (Decode.field "runsElapsed" Decode.int)
        (Decode.field "badLabel" Decode.string)
        (Decode.field "badLabelPercentage" Decode.float)
        (Decode.field "expectedDistribution" Decode.string)


decodeRecord_expected_String_actual_String_extra_ListString_missing_ListString_ =
    Decode.map4
        Record_expected_String_actual_String_extra_ListString_missing_ListString_
        (Decode.field "expected" Decode.string)
        (Decode.field "actual" Decode.string)
        (Decode.field "extra" (Decode.list Decode.string))
        (Decode.field "missing" (Decode.list Decode.string))


decodeRecord_labels_ListString_duration_Float_logs_ListString_distributionReports_ListDistributionReport_ =
    Decode.map4
        Record_labels_ListString_duration_Float_logs_ListString_distributionReports_ListDistributionReport_
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.field "duration" Decode.float)
        (Decode.field "logs" (Decode.list Decode.string))
        (Decode.field "distributionReports" (Decode.list decodeDistributionReport))


decodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_distributionReports_ListDistributionReport_ =
    Decode.map6
        Record_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_distributionReports_ListDistributionReport_
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.field "duration" Decode.float)
        (Decode.field "logs" (Decode.list Decode.string))
        (Decode.field "todos" (Decode.list Decode.string))
        (Decode.field "failures" (Decode.list decodeFailure))
        (Decode.field "distributionReports" (Decode.list decodeDistributionReport))


decodeTestResult : Decoder TestResult
decodeTestResult =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeTestResultHelp


decodeTestResultHelp constructor =
    case constructor of
        "Passed" ->
            Decode.map
                Passed
                (Decode.field "A1" decodeRecord_labels_ListString_duration_Float_logs_ListString_distributionReports_ListDistributionReport_)

        "Failed" ->
            Decode.map
                Failed
                (Decode.field "A1" decodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_distributionReports_ListDistributionReport_)

        other ->
            Decode.fail <| "Unknown constructor for type TestResult: " ++ other


encodeDistributionReport : DistributionReport -> Value
encodeDistributionReport a =
    case a of
        NoDistribution ->
            Encode.object
                [ ( "Constructor", Encode.string "NoDistribution" )
                ]

        DistributionToReport a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "DistributionToReport" )
                , ( "A1", encodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_ a1 )
                ]

        DistributionCheckSucceeded a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "DistributionCheckSucceeded" )
                , ( "A1", encodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_ a1 )
                ]

        DistributionCheckFailed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "DistributionCheckFailed" )
                , ( "A1", encodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedDistribution_String_ a1 )
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


encodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_ a =
    Encode.object
        [ ( "distributionCount", encodeDict_ListString_Int a.distributionCount )
        , ( "runsElapsed", Encode.int a.runsElapsed )
        ]


encodeRecord_distributionCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedDistribution_String_ a =
    Encode.object
        [ ( "distributionCount", encodeDict_ListString_Int a.distributionCount )
        , ( "runsElapsed", Encode.int a.runsElapsed )
        , ( "badLabel", Encode.string a.badLabel )
        , ( "badLabelPercentage", Encode.float a.badLabelPercentage )
        , ( "expectedDistribution", Encode.string a.expectedDistribution )
        ]


encodeRecord_expected_String_actual_String_extra_ListString_missing_ListString_ a =
    Encode.object
        [ ( "expected", Encode.string a.expected )
        , ( "actual", Encode.string a.actual )
        , ( "extra", Encode.list Encode.string a.extra )
        , ( "missing", Encode.list Encode.string a.missing )
        ]


encodeRecord_labels_ListString_duration_Float_logs_ListString_distributionReports_ListDistributionReport_ a =
    Encode.object
        [ ( "labels", Encode.list Encode.string a.labels )
        , ( "duration", Encode.float a.duration )
        , ( "logs", Encode.list Encode.string a.logs )
        , ( "distributionReports", Encode.list encodeDistributionReport a.distributionReports )
        ]


encodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_distributionReports_ListDistributionReport_ a =
    Encode.object
        [ ( "labels", Encode.list Encode.string a.labels )
        , ( "duration", Encode.float a.duration )
        , ( "logs", Encode.list Encode.string a.logs )
        , ( "todos", Encode.list Encode.string a.todos )
        , ( "failures", Encode.list encodeFailure a.failures )
        , ( "distributionReports", Encode.list encodeDistributionReport a.distributionReports )
        ]


encodeTestResult : TestResult -> Value
encodeTestResult a =
    case a of
        Passed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Passed" )
                , ( "A1", encodeRecord_labels_ListString_duration_Float_logs_ListString_distributionReports_ListDistributionReport_ a1 )
                ]

        Failed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Failed" )
                , ( "A1", encodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_distributionReports_ListDistributionReport_ a1 )
                ]
