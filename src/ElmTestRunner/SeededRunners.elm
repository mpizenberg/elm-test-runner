module ElmTestRunner.SeededRunners exposing (SeededRunners, Kind(..), empty, fromTest, getKind, getTestsCount, run, kindFromString, kindToString)

{-| Helper module to prepare and run test runners.

@docs SeededRunners, Kind, empty, fromTest, getKind, getTestsCount, run, kindFromString, kindToString

-}

import Array exposing (Array)
import ElmTestRunner.Result exposing (TestResult(..))
import Random
import Task exposing (Task)
import Test exposing (Test)
import Test.Distribution exposing (DistributionReport(..))
import Test.Runner.Failure exposing (Reason(..))
import Test.RunnerV2 exposing (FuzzTest, Tests, UnitTest)


{-| Tests with a random seed and the number of fuzz runs.
The type tells us if `Test.only` or `Test.skip` was used,
and provides the tests in arrays for efficient indexed access.
-}
type SeededRunners
    = SeededRunners Random.Seed Int Kind (Array UnitTest) (Array FuzzTest)


{-| Informs us if `Test.only` or `Test.skip` was used.
-}
type Kind
    = Plain
    | Only
    | Skipping


{-| Parse a kind from a String.
-}
kindFromString : String -> Result String Kind
kindFromString input =
    case input of
        "Plain" ->
            Ok Plain

        "Only" ->
            Ok Only

        "Skipping" ->
            Ok Skipping

        _ ->
            Err input


{-| Serialize a kind to a String.
-}
kindToString : Kind -> String
kindToString kind =
    case kind of
        Plain ->
            "Plain"

        Only ->
            "Only"

        Skipping ->
            "Skipping"


{-| Create an empty SeededRunners when there isn't any test
-}
empty : SeededRunners
empty =
    SeededRunners (Random.initialSeed 0) 0 Plain Array.empty Array.empty


{-| Convert a "master" test into a `SeededRunners`.
That "master" test usually is the concatenation of all exposed tests.
-}
fromTest : Test -> { initialSeed : Int, fuzzRuns : Int, filter : Maybe String } -> SeededRunners
fromTest masterTest { initialSeed, fuzzRuns, filter } =
    let
        seed =
            Random.initialSeed initialSeed

        tests =
            Test.RunnerV2.toTests masterTest

        unitTests =
            Test.RunnerV2.getUnitTests tests

        fuzzTests =
            Test.RunnerV2.getFuzzTests tests
    in
    case Test.RunnerV2.getExcludedDueToOnly tests of
        Just _ ->
            SeededRunners seed fuzzRuns Only unitTests fuzzTests

        Nothing ->
            SeededRunners
                seed
                fuzzRuns
                (if Test.RunnerV2.getExcludedDueToSkip tests > 0 then
                    Skipping

                 else
                    Plain
                )
                (filterTests filter Test.RunnerV2.getUnitTestLabels unitTests)
                (filterTests filter Test.RunnerV2.getFuzzTestLabels fuzzTests)


{-| Get the `Kind` of tests.
-}
getKind : SeededRunners -> Kind
getKind (SeededRunners _ _ kind _ _) =
    kind


{-| Get the number of tests.
-}
getTestsCount : SeededRunners -> Int
getTestsCount (SeededRunners _ _ _ unitTests fuzzTests) =
    Array.length unitTests + Array.length fuzzTests


filterTests : Maybe String -> (test -> List String) -> Array test -> Array test
filterTests filter getLabels tests =
    case filter of
        Nothing ->
            tests

        Just pattern ->
            Array.filter (\r -> List.any (String.contains pattern) (getLabels r)) tests


{-| Run a given test if the id is in range.
-}
run : Int -> SeededRunners -> Maybe (Task Never TestResult)
run id (SeededRunners seed fuzzRuns _ unitTests fuzzTests) =
    case Array.get id unitTests of
        Just unitTest ->
            Test.RunnerV2.runUnitTest unitTest
                |> Task.map
                    (\( unitTestExpectation, duration, debugLogs ) ->
                        case unitTestExpectation of
                            Test.RunnerV2.UnitTestPass ->
                                Passed
                                    { labels = Test.RunnerV2.getUnitTestLabels unitTest
                                    , duration = duration
                                    , logs = debugLogs
                                    , distributionReports = [ NoDistribution () ]
                                    }

                            Test.RunnerV2.UnitTestFail unitTestFailData ->
                                let
                                    ( todos, failures ) =
                                        case Test.RunnerV2.getUnitTestFailReason unitTestFailData of
                                            TODO ->
                                                ( [ Test.RunnerV2.getUnitTestFailDescription unitTestFailData ]
                                                , []
                                                )

                                            reason ->
                                                ( []
                                                , [ { given = Nothing
                                                    , description = Test.RunnerV2.getUnitTestFailDescription unitTestFailData
                                                    , reason = reason
                                                    }
                                                  ]
                                                )
                                in
                                Failed
                                    { labels = Test.RunnerV2.getUnitTestLabels unitTest
                                    , duration = duration
                                    , logs = debugLogs
                                    , todos = todos
                                    , failures = failures
                                    , distributionReports = [ NoDistribution () ]
                                    }
                    )
                |> Just

        Nothing ->
            case Array.get (id - Array.length unitTests) fuzzTests of
                Just fuzzTest ->
                    Test.RunnerV2.runFuzzTest fuzzTest seed fuzzRuns []
                        |> Task.map
                            (\( fuzzTestExpectation, duration, debugLogs ) ->
                                case fuzzTestExpectation of
                                    Test.RunnerV2.FuzzTestPass fuzzTestPassData ->
                                        Passed
                                            { labels = Test.RunnerV2.getFuzzTestLabels fuzzTest
                                            , duration = duration
                                            , logs = debugLogs
                                            , distributionReports = [ Test.RunnerV2.getFuzzTestPassDistributionReport fuzzTestPassData ]
                                            }

                                    Test.RunnerV2.FuzzTestFail fuzzTestFailData ->
                                        Failed
                                            { labels = Test.RunnerV2.getFuzzTestLabels fuzzTest
                                            , duration = duration
                                            , logs = debugLogs
                                            , todos = []
                                            , failures =
                                                [ { given = Test.RunnerV2.getFuzzTestFailGiven fuzzTestFailData
                                                  , description = Test.RunnerV2.getFuzzTestFailDescription fuzzTestFailData
                                                  , reason = Test.RunnerV2.getFuzzTestFailReason fuzzTestFailData
                                                  }
                                                ]
                                            , distributionReports = [ Test.RunnerV2.getFuzzTestFailDistributionReport fuzzTestFailData ]
                                            }
                            )
                        |> Just

                Nothing ->
                    Nothing
