module ElmTestRunner.Result exposing
    ( TestResult(..), fromExpectations, setDuration, setLogs, encode, decoder
    , Summary, summary, encodeCoverageReport
    )

{-| Types and functions to manipulate a test result.


# Manipulation of the result of a test run

@docs TestResult, fromExpectations, setDuration, setLogs, encode, decoder


# Helper functions

@docs Summary, summary, encodeCoverageReport

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
        , successes : List CoverageReport -- TODO change to success : CoverageReport
        }
    | Failed
        { labels : List String
        , duration : Float
        , logs : List String
        , todos : List String
        , failures : List ( Failure, CoverageReport )
        }


{-| Set the duration that the test took.
-}
setDuration : Float -> TestResult -> TestResult
setDuration duration testResult =
    case testResult of
        Passed r ->
            Passed { r | duration = duration }

        Failed r ->
            Failed { r | duration = duration }


{-| Set the logs received for that test.
-}
setLogs : List String -> TestResult -> TestResult
setLogs logs testResult =
    case testResult of
        Passed r ->
            Passed { r | logs = logs }

        Failed r ->
            Failed { r | logs = logs }


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
            , successes = outcomes.successes
            }

    else
        Failed
            { labels = labels
            , duration = 0
            , logs = []
            , todos = outcomes.todos
            , failures = outcomes.failures
            }


type alias Outcomes =
    { todos : List String
    , failures : List ( Failure, CoverageReport )
    , successes : List CoverageReport
    }


initOutcomes : Outcomes
initOutcomes =
    { todos = []
    , failures = []
    , successes = []
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
            { outcomes | successes = coverageReport :: outcomes.successes }

        Just failure ->
            if Test.Runner.isTodo expectation then
                { outcomes | todos = failure.description :: outcomes.todos }

            else
                { outcomes | failures = ( failure, coverageReport ) :: outcomes.failures }


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


type alias Record_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_List_Failure_CoverageReport__ =
    { labels : List String, duration : Float, logs : List String, todos : List String, failures : List ( Failure, CoverageReport ) }


type alias Record_labels_ListString_duration_Float_logs_ListString_successes_ListCoverageReport_ =
    { labels : List String, duration : Float, logs : List String, successes : List CoverageReport }


decodeRecord_labels_ListString_duration_Float_logs_ListString_successes_ListCoverageReport_ =
    Decode.map4
        Record_labels_ListString_duration_Float_logs_ListString_successes_ListCoverageReport_
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.field "duration" Decode.float)
        (Decode.field "logs" (Decode.list Decode.string))
        (Decode.field "successes" (Decode.list decodeCoverageReport))


decodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_List_Failure_CoverageReport__ =
    Decode.map5
        Record_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_List_Failure_CoverageReport__
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.field "duration" Decode.float)
        (Decode.field "logs" (Decode.list Decode.string))
        (Decode.field "todos" (Decode.list Decode.string))
        (Decode.field "failures" (Decode.list decodeTuple_Failure_CoverageReport_))


decodeTestResult : Decoder TestResult
decodeTestResult =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeTestResultHelp


decodeTestResultHelp constructor =
    case constructor of
        "Passed" ->
            Decode.map
                Passed
                (Decode.field "A1" decodeRecord_labels_ListString_duration_Float_logs_ListString_successes_ListCoverageReport_)

        "Failed" ->
            Decode.map
                Failed
                (Decode.field "A1" decodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_List_Failure_CoverageReport__)

        other ->
            Decode.fail <| "Unknown constructor for type TestResult: " ++ other


decodeTuple_Failure_CoverageReport_ : Decoder ( Failure, CoverageReport )
decodeTuple_Failure_CoverageReport_ =
    Decode.map2
        (\a1 a2 -> ( a1, a2 ))
        (Decode.field "A1" decodeFailure)
        (Decode.field "A2" decodeCoverageReport)


encodeRecord_labels_ListString_duration_Float_logs_ListString_successes_ListCoverageReport_ a =
    Encode.object
        [ ( "labels", Encode.list Encode.string a.labels )
        , ( "duration", Encode.float a.duration )
        , ( "logs", Encode.list Encode.string a.logs )
        , ( "successes", Encode.list encodeCoverageReport a.successes )
        ]


encodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_List_Failure_CoverageReport__ a =
    Encode.object
        [ ( "labels", Encode.list Encode.string a.labels )
        , ( "duration", Encode.float a.duration )
        , ( "logs", Encode.list Encode.string a.logs )
        , ( "todos", Encode.list Encode.string a.todos )
        , ( "failures", Encode.list encodeTuple_Failure_CoverageReport_ a.failures )
        ]


encodeTestResult : TestResult -> Value
encodeTestResult a =
    case a of
        Passed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Passed" )
                , ( "A1", encodeRecord_labels_ListString_duration_Float_logs_ListString_successes_ListCoverageReport_ a1 )
                ]

        Failed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Failed" )
                , ( "A1", encodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_List_Failure_CoverageReport__ a1 )
                ]


encodeTuple_Failure_CoverageReport_ : ( Failure, CoverageReport ) -> Value
encodeTuple_Failure_CoverageReport_ ( a1, a2 ) =
    Encode.object
        [ ( "A1", encodeFailure a1 )
        , ( "A2", encodeCoverageReport a2 )
        ]


type alias Record_coverageCount_Dict_ListString_Int_runsElapsed_Int_badLabel_String_badLabelPercentage_Float_expectedCoverage_String_ =
    { coverageCount : Dict (List String) Int, runsElapsed : Int, badLabel : String, badLabelPercentage : Float, expectedCoverage : String }


type alias Record_coverageCount_Dict_ListString_Int_runsElapsed_Int_ =
    { coverageCount : Dict (List String) Int, runsElapsed : Int }


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
