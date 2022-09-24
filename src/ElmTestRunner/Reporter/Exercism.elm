module ElmTestRunner.Reporter.Exercism exposing (implementation)

{-| Implementation of a reporter following the spec
for exercism tests runners.

<https://github.com/exercism/docs/blob/main/building/tooling/test-runners/interface.md>

@docs implementation

-}

import Array exposing (Array)
import ElmTestRunner.Failure exposing (Failure)
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (Summary, TestResult)
import ElmTestRunner.SeededRunners exposing (Kind(..))
import ElmTestRunner.Vendor.ConsoleFormat exposing (format)
import ElmTestRunner.Vendor.ConsoleText as Text exposing (Text, UseColor)
import ElmTestRunner.Vendor.FormatColor as FormatColor
import ElmTestRunner.Vendor.FormatMonochrome as FormatMonochrome
import Json.Encode as Encode exposing (Value)
import Test.Coverage exposing (CoverageReport)


{-| Implementation of a reporter for exercism, mostly for automated tools.
-}
implementation : Interface
implementation =
    { onBegin = always Nothing
    , onResult = always Nothing
    , onEnd = \kind result -> Just (summary kind result)
    }



-- {
--   "version": 3,
--   "status": "fail",
--   "message": null,
--   "tests": [
--     {
--       "name": "1 > Test that the thing works",
--       "task_id": 1,
--       "status": "fail",
--       "message": "Expected 42 but got 123123",
--       "output": "Debugging information output by the user",
--       "test_code": "assert_equal 42, answerToTheUltimateQuestion()"
--     }
--   ]
-- }


summary : Result String Kind -> Array TestResult -> String
summary kindOrErr results =
    case kindOrErr of
        Err err ->
            Encode.encode 2
                (Encode.object
                    [ ( "version", Encode.int 3 )
                    , ( "status", Encode.string "error" )
                    , ( "message", Encode.string err )
                    ]
                )

        Ok kind ->
            let
                status =
                    summaryStatus kind (TestResult.summary results)

                tests =
                    Array.map toExercismResult results
            in
            Encode.encode 2
                (Encode.object
                    [ ( "version", Encode.int 3 )
                    , ( "status", Encode.string status )
                    , ( "tests", Encode.array encodeExercismResult tests )
                    ]
                )


summaryStatus : Kind -> Summary -> String
summaryStatus kind { failedCount, todoCount } =
    case ( kind, failedCount, todoCount ) of
        ( Plain, 0, 0 ) ->
            "pass"

        _ ->
            "fail"


type alias ExercismResult =
    { name : String
    , taskId : Maybe Int
    , status : String
    , message : Maybe String
    , output : Maybe String
    }


encodeExercismResult : ExercismResult -> Value
encodeExercismResult { name, taskId, status, message, output } =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "task_id", Maybe.withDefault Encode.null (Maybe.map Encode.int taskId) )
        , ( "status", Encode.string status )
        , ( "message", Maybe.withDefault Encode.null (Maybe.map Encode.string message) )
        , ( "output", Maybe.withDefault Encode.null (Maybe.map Encode.string output) )
        , ( "test_code", Encode.null )
        ]


toExercismResult : TestResult -> ExercismResult
toExercismResult testResult =
    case testResult of
        TestResult.Passed { labels } ->
            { name = extractTestName labels
            , taskId = extractTaskId labels
            , status = "pass"
            , message = Nothing
            , output = Nothing
            }

        TestResult.Failed { labels, failures, todos, logs } ->
            { name = extractTestName labels
            , taskId = extractTaskId labels
            , status = "fail"
            , message = Just (failureMessage failures todos)
            , output =
                case logs of
                    [] ->
                        Nothing

                    _ ->
                        Just (String.join "\n" logs)
            }


{-| Use all description levels, except the first one (exercise name), to build the test name.
-}
extractTestName : List String -> String
extractTestName labels =
    List.reverse labels
        |> List.drop 1
        |> String.join " > "


{-| Extract the task id as one of the description levels
containing a single number identifying the task id.
-}
extractTaskId : List String -> Maybe Int
extractTaskId labels =
    List.filterMap String.toInt labels
        |> List.head


failureMessage : List ( Failure, CoverageReport ) -> List String -> String
failureMessage failures todos =
    let
        useColor =
            Text.Monochrome
    in
    if not (List.isEmpty failures) then
        List.map (failureToText useColor) failures
            |> Text.concat
            |> Text.render useColor

    else
        String.join "\n" todos


failureToText : UseColor -> ( Failure, CoverageReport ) -> Text
failureToText useColor ( { given, description, reason }, _ ) =
    let
        formatEquality =
            case useColor of
                Text.Monochrome ->
                    FormatMonochrome.formatEquality

                Text.UseColor ->
                    FormatColor.formatEquality

        messageText =
            Text.plain ("\n" ++ indent (format formatEquality description reason) ++ "\n\n")
    in
    case given of
        Nothing ->
            messageText

        Just givenStr ->
            [ Text.dark (Text.plain ("\nGiven " ++ givenStr ++ "\n"))
            , messageText
            ]
                |> Text.concat


indent : String -> String
indent str =
    String.split "\n" str
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"
