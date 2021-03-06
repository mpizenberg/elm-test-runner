module ElmTestRunner.Reporter.Interface exposing (Interface)

{-| Reporter interface.

@docs Interface

-}

import Array exposing (Array)
import ElmTestRunner.Result exposing (TestResult)
import ElmTestRunner.SeededRunners exposing (Kind)


{-| Interface that must be implemented by a reporter.
The return types are `Maybe String` to know if something (or not)
has to be logged by the reporter.

    onBegin : Int -> Maybe String
    onBegin testsCount = ...

    onResult : TestResult -> Maybe String
    onResult testResult = ...

    onEnd : Result String Kind -> Array TestResult -> Maybe String
    onEnd kindResult allTestResults = ...

-}
type alias Interface =
    { onBegin : Int -> Maybe String
    , onResult : TestResult -> Maybe String
    , onEnd : Result String Kind -> Array TestResult -> Maybe String
    }
