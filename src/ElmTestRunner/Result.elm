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
import ElmTestRunner.Failure as Failure exposing (Failure)
import Expect exposing (Expectation)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Test.Runner


{-| Type summarizing the results of a test run.
It is obtained from the list of expectations returned by calling runner.run ().
-}
type TestResult
    = Passed { labels : List String, duration : Float, logs : List String }
    | Failed { labels : List String, duration : Float, logs : List String, todos : List String, failures : List Failure }


{-| Set the duration that the test took.
-}
setDuration : Float -> TestResult -> TestResult
setDuration duration testResult =
    case testResult of
        Passed { labels, logs } ->
            Passed { labels = labels, duration = duration, logs = logs }

        Failed { labels, logs, todos, failures } ->
            Failed { labels = labels, duration = duration, logs = logs, todos = todos, failures = failures }


{-| Set the logs received for that test.
-}
setLogs : List String -> TestResult -> TestResult
setLogs logs testResult =
    case testResult of
        Passed { labels, duration } ->
            Passed { labels = labels, duration = duration, logs = logs }

        Failed { labels, duration, todos, failures } ->
            Failed { labels = labels, duration = duration, todos = todos, failures = failures, logs = logs }


{-| Convert a list of expectations (results of a run) into a `TestResult`.
Return the `Failed` variant if there is any todo or failure in the expectations.
-}
fromExpectations : List String -> List Expectation -> TestResult
fromExpectations labels expectations =
    case failuresAndTodos expectations of
        ( [], [] ) ->
            Passed { labels = labels, duration = 0, logs = [] }

        ( todos, failures ) ->
            Failed { labels = labels, duration = 0, todos = todos, failures = failures, logs = [] }


failuresAndTodos : List Expectation -> ( List String, List Failure )
failuresAndTodos expectations =
    List.foldl accumFailuresAndTodos ( [], [] ) expectations


accumFailuresAndTodos : Expectation -> ( List String, List Failure ) -> ( List String, List Failure )
accumFailuresAndTodos expectation (( todos, failures ) as outcomes) =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            outcomes

        Just failure ->
            if Test.Runner.isTodo expectation then
                ( failure.description :: todos, failures )

            else
                ( todos, failure :: failures )


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


type alias Record_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_ =
    { labels : List String, duration : Float, logs : List String, todos : List String, failures : List Failure }


type alias Record_labels_ListString_duration_Float_logs_ListString_ =
    { labels : List String, duration : Float, logs : List String }


decodeRecord_labels_ListString_duration_Float_logs_ListString_ =
    Decode.map3
        Record_labels_ListString_duration_Float_logs_ListString_
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.field "duration" Decode.float)
        (Decode.field "logs" (Decode.list Decode.string))


decodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_ =
    Decode.map5
        Record_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.field "duration" Decode.float)
        (Decode.field "logs" (Decode.list Decode.string))
        (Decode.field "todos" (Decode.list Decode.string))
        (Decode.field "failures" (Decode.list decodeFailure))


decodeTestResult =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeTestResultHelp


decodeTestResultHelp constructor =
    case constructor of
        "Passed" ->
            Decode.map
                Passed
                (Decode.field "A1" decodeRecord_labels_ListString_duration_Float_logs_ListString_)

        "Failed" ->
            Decode.map
                Failed
                (Decode.field "A1" decodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_)

        other ->
            Decode.fail <| "Unknown constructor for type TestResult: " ++ other


encodeRecord_labels_ListString_duration_Float_logs_ListString_ a =
    Encode.object
        [ ( "labels", Encode.list Encode.string a.labels )
        , ( "duration", Encode.float a.duration )
        , ( "logs", Encode.list Encode.string a.logs )
        ]


encodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_ a =
    Encode.object
        [ ( "labels", Encode.list Encode.string a.labels )
        , ( "duration", Encode.float a.duration )
        , ( "logs", Encode.list Encode.string a.logs )
        , ( "todos", Encode.list Encode.string a.todos )
        , ( "failures", Encode.list encodeFailure a.failures )
        ]


encodeTestResult a =
    case a of
        Passed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Passed" )
                , ( "A1", encodeRecord_labels_ListString_duration_Float_logs_ListString_ a1 )
                ]

        Failed a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Failed" )
                , ( "A1", encodeRecord_labels_ListString_duration_Float_logs_ListString_todos_ListString_failures_ListFailure_ a1 )
                ]
