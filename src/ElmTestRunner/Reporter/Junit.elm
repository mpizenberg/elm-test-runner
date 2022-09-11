module ElmTestRunner.Reporter.Junit exposing (implementation)

{-| Json implementation of a reporter

@docs implementation

The output format is based on the xsd specification found at
<https://github.com/windyroad/JUnit-Schema/blob/master/JUnit.xsd>

-}

import Array exposing (Array)
import Dict
import ElmTestRunner.Failure exposing (Failure)
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Result as TestResult exposing (TestResult)
import ElmTestRunner.SeededRunners exposing (Kind(..))
import ElmTestRunner.Vendor.XmlEncode as Encode exposing (Value)
import Test.Coverage exposing (CoverageReport)
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| Provide a Junit XML implementation of a reporter, mostly for automated tools.
-}
implementation : Interface
implementation =
    { onBegin = always Nothing
    , onResult = always Nothing
    , onEnd = \kind result -> Just (summary kind result)
    }


{-| See spec for "testsuite" here:
<https://github.com/windyroad/JUnit-Schema/blob/master/JUnit.xsd>
-}
summary : Result String Kind -> Array TestResult -> String
summary kind results =
    let
        { totalDuration, failedCount, todoCount } =
            TestResult.summary results

        encodedTests =
            Array.toList results
                |> List.map encodeTestResult
                |> Encode.list

        skipped =
            case kind of
                Ok Only ->
                    1

                Ok Skipping ->
                    1

                _ ->
                    0

        suiteAttributes =
            Dict.fromList
                -- No "package" key since these are only used in aggregated "testsuites".
                -- Full class name of the test for non-aggregated testsuite documents.
                -- Class name without the package for aggregated testsuites documents.
                [ ( "name", Encode.string "elm-test-rs" )

                -- When the test was executed. Timezone may not be specified.
                -- type="ISO8601_DATETIME_PATTERN"
                -- , ( "timestamp", "required" )
                --
                -- Host on which the tests were executed. 'localhost' should be used if the hostname cannot be determined.
                , ( "hostname", Encode.string "localhost" )

                -- The total number of tests in the suite
                , ( "tests", Encode.int (Array.length results) )

                -- "failures" should be used and not "failed" as in elm-test.
                -- The total number of tests in the suite that failed.
                -- A failure is a test which the code has explicitly failed by using the mechanisms for that purpose.
                , ( "failures", Encode.int failedCount )

                -- The total number of tests in the suite that errored.
                -- An errored test is one that had an unanticipated problem.
                -- In our case, all errors are aggregated as failures by elm-explorations/test so we just hardcode 0.
                , ( "errors", Encode.int 0 )

                -- The total number of ignored or skipped tests in the suite.
                -- In our case, we can set this to 1 if Test.skip or Test.only was used.
                , ( "skipped", Encode.int skipped )

                -- Time taken (in seconds) to execute the tests in the suite.
                , ( "time", Encode.float (totalDuration / 1000) )
                ]
    in
    Encode.encode 0 <|
        Encode.list
            [ Encode.string "<?xml version=\"1.0\"?>"
            , Encode.object [ ( "testsuite", suiteAttributes, encodedTests ) ]
            ]


{-| See spec for "testcase" here:
<https://github.com/windyroad/JUnit-Schema/blob/master/JUnit.xsd>
-}
encodeTestResult : TestResult -> Encode.Value
encodeTestResult result =
    let
        { labels, duration, failures, logs } =
            case result of
                TestResult.Passed test ->
                    { labels = test.labels
                    , duration = test.duration
                    , failures = Encode.null
                    , logs = test.logs
                    }

                TestResult.Failed test ->
                    { labels = test.labels
                    , duration = test.duration
                    , failures = encodeFailures test.failures test.todos
                    , logs = test.logs
                    }

        ( class, name ) =
            classAndName labels

        attributesDict =
            Dict.fromList
                -- Full class name for the class the test method is in.
                -- In our case we just aggregate all parts except last of the test description.
                [ ( "classname", Encode.string class )

                -- Name of the test method.
                -- In our case the last part of the test description.
                , ( "name", Encode.string name )

                -- Time taken (in seconds) to execute the test.
                , ( "time", Encode.float (duration / 1000) )

                -- Data that was written to standard out while the test was executed.
                -- Normally this is not an attribute inside "testcase" but a tag inside "testsuite" but here we bend the rules
                -- to get the Debug.log outputs for this particular test.
                , ( "system-out", Encode.string (String.join "\n" logs) )
                ]
    in
    Encode.object
        [ ( "testcase", attributesDict, failures ) ]


classAndName : List String -> ( String, String )
classAndName labels =
    case labels of
        [] ->
            ( "", "" )

        name :: classLabels ->
            ( String.join " > " (List.reverse classLabels), name )


coverageReportToString : CoverageReport -> Maybe String
coverageReportToString coverageReport =
    case coverageReport of
        Test.Coverage.NoCoverage ->
            Nothing

        Test.Coverage.CoverageToReport r ->
            Just (Test.Coverage.coverageReportTable r)

        Test.Coverage.CoverageCheckSucceeded _ ->
            {- Not reporting the table to the JUnit stdout (similarly to the
               Console reporter) although the data is technically there.
               We keep the full data dump for the JSON reporter.
            -}
            Nothing

        Test.Coverage.CoverageCheckFailed r ->
            Just (Test.Coverage.coverageReportTable r)


{-| See spec for "failure" here:
<https://github.com/windyroad/JUnit-Schema/blob/master/JUnit.xsd>
-}
encodeFailures : List ( Failure, CoverageReport ) -> List String -> Encode.Value
encodeFailures failures todos =
    let
        message : String
        message =
            if not (List.isEmpty failures) then
                List.map (Tuple.first >> formatFailure) failures
                    |> String.join "\n\n\n"

            else
                List.map ((++) "TODO: ") todos
                    |> String.join "\n"

        stdout : Maybe String
        stdout =
            case List.filterMap (Tuple.second >> coverageReportToString) failures of
                [] ->
                    Nothing

                list ->
                    Just (String.join "\n\n\n" list)

        attributesDict =
            [ -- The message specified in the assert.
              Just ( "message", Encode.string message )

            -- stdout - currently used for coverage reports
            , stdout |> Maybe.map (\out -> ( "system-out", Encode.string out ))

            -- The type of the assert.
            -- This is unknown for Elm tests.
            -- , ( "type", "required" )
            ]
                |> List.filterMap identity
                |> Dict.fromList
    in
    Encode.object [ ( "failure", attributesDict, Encode.null ) ]


formatFailure : Failure -> String
formatFailure { given, description, reason } =
    let
        message =
            reasonToString description reason
    in
    case given of
        Just str ->
            "Given " ++ str ++ "\n\n" ++ message

        Nothing ->
            message


reasonToString : String -> Reason -> String
reasonToString description reason =
    case reason of
        Custom ->
            description

        Equality expected actual ->
            expected ++ "\n\nwas not equal to\n\n" ++ actual

        Comparison first second ->
            first ++ "\n\nfailed when compared with " ++ description ++ " on\n\n" ++ second

        TODO ->
            "TODO: " ++ description

        Invalid BadDescription ->
            let
                explanation =
                    if description == "" then
                        "The empty string is not a valid test description."

                    else
                        "This is an invalid test description: " ++ description
            in
            "Invalid test: " ++ explanation

        Invalid _ ->
            "Invalid test: " ++ description

        ListDiff expected actual ->
            String.join ", " expected ++ "\n\nhad different elements than\n\n" ++ String.join ", " actual

        CollectionDiff { expected, actual, extra, missing } ->
            expected
                ++ "\n\nhad different contents than\n\n"
                ++ actual
                ++ "\n\nthese were extra:\n\n"
                ++ String.join "\n" extra
                ++ "\n\nthese were missing:\n\n"
                ++ String.join "\n" missing
