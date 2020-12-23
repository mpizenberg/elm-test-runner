module ElmTestRunner.SeededRunners exposing (SeededRunners, Kind(..), empty, fromTest, run, kindFromString, kindToString)

{-| Helper module to prepare and run test runners.

@docs SeededRunners, Kind, empty, fromTest, run, kindFromString, kindToString

-}

import Array exposing (Array)
import ElmTestRunner.Result as TestResult exposing (TestResult)
import Random
import Test exposing (Test)
import Test.Runner exposing (Runner)


{-| Runners prepared with their random seed.
If runners are invalid for some reason (duplicate name, ...),
this is will be an `Err String`.
Otherwise, the type tells us if `Test.only` or `Test.skip` was used,
and provides the seeded runners in an array for efficient indexed access.
-}
type alias SeededRunners =
    Result String { kind : Kind, runners : Array Runner }


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
    Ok { kind = Plain, runners = Array.empty }


{-| Convert a "master" test into seeded runners.
That "master" test usually is the concatenation of all exposed tests.
-}
fromTest : Test -> { initialSeed : Int, fuzzRuns : Int, filter : Maybe String } -> SeededRunners
fromTest masterTest { initialSeed, fuzzRuns, filter } =
    case Test.Runner.fromTest fuzzRuns (Random.initialSeed initialSeed) masterTest of
        Test.Runner.Plain runnerList ->
            Ok { kind = Plain, runners = Array.fromList (filterRunners filter runnerList) }

        Test.Runner.Only runnerList ->
            Ok { kind = Only, runners = Array.fromList runnerList }

        Test.Runner.Skipping runnerList ->
            Ok { kind = Skipping, runners = Array.fromList (filterRunners filter runnerList) }

        Test.Runner.Invalid error ->
            Err error


filterRunners : Maybe String -> List Runner -> List Runner
filterRunners filter runners =
    case filter of
        Nothing ->
            runners

        Just pattern ->
            List.filter (\r -> List.any (String.contains pattern) r.labels) runners


{-| Run a given test if the id is in range.
-}
run : Int -> Array Runner -> Maybe TestResult
run id runners =
    Array.get id runners
        |> Maybe.map (\runner -> { labels = runner.labels, expectations = runner.run () })
        |> Maybe.map (\result -> TestResult.fromExpectations result.labels result.expectations)
