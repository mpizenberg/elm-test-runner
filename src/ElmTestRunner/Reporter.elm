module ElmTestRunner.Reporter exposing
    ( worker, Ports
    , Flags, Model, Msg
    )

{-| Main module for a test reporter.


# Create a main test reporter

@docs worker, Ports


# Internal types for function signatures

@docs Flags, Model, Msg

-}

import Array exposing (Array)
import ElmTestRunner.Reporter.Console as ReporterConsole
import ElmTestRunner.Reporter.ConsoleBis as ReporterConsoleBis
import ElmTestRunner.Reporter.Interface exposing (Interface)
import ElmTestRunner.Reporter.Json as ReporterJson
import ElmTestRunner.Reporter.Junit as ReporterJunit
import ElmTestRunner.Result as TestResult exposing (TestResult)
import ElmTestRunner.SeededRunners as SeededRunners exposing (kindFromString)
import Json.Decode exposing (Value, decodeValue)
import Process
import Task


{-| Ports(ish) required by the worker program to function.
They aren't necessarily exactly ports
but will basically be wrapped by an actual port in the main Elm caller module.
-}
type alias Ports msg =
    { restart : ({ kind : String, testsCount : Int } -> msg) -> Sub msg
    , incomingResult : ({ duration : Float, result : Value, logs : List String } -> msg) -> Sub msg
    , stdout : String -> Cmd msg
    , signalFinished : { exitCode : Int, testsCount : Int } -> Cmd msg
    }


{-| Create a tests reporter.
Some specific ports(ish) are required as arguments,
The main Elm module calling this one will typically look like the example below.

    port module Reporter exposing (main)

    import ElmTestRunner.Reporter exposing (Flags, Model, Msg)
    import Json.Decode exposing (Value)

    port restart : ({ kind : String, count : Int } -> msg) -> Sub msg

    port incomingResult : ({ duration : Float, result : Value, logs : List String } -> msg) -> Sub msg

    port signalFinished : { exitCode : Int, testsCount : Int } -> Cmd msg

    port stdout : String -> Cmd msg

    main : Program Flags Model Msg
    main =
        ElmTestRunner.Reporter.worker
            { restart = restart
            , incomingResult = incomingResult
            , stdout = stdout
            , signalFinished = signalFinished
            }

It can later be called with a tiny bit of JS similar to:

```js
// Start the Elm app
const { Elm } = require("./Reporter.elm.js");
const flags = {
  initialSeed: {{ initialSeed }},
  fuzzRuns: {{ fuzzRuns }},
  mode: "{{ reporter }}",
};
const app = Elm.Reporter.init({ flags: flags });

// Pipe the Elm stdout port to stdout
app.ports.stdout.subscribe((str) => process.stdout.write(str));

// Export function to set the callback function when reports are finished
let finishCallback = () => console.error("finishCallback not defined yet");
app.ports.signalFinished.subscribe(({exitCode}) => finishCallback(exitCode));
exports.setCallback = (callback) => { finishCallback = callback; };

// Export function to restart the Elm reporter
exports.restart = app.ports.restart.send;

// Export function to send results to Elm
exports.sendResult = app.ports.incomingResult.send;
```

-}
worker : Ports Msg -> Program Flags Model Msg
worker ({ restart, incomingResult } as ports) =
    Platform.worker
        { init = init ports
        , update = update
        , subscriptions = \_ -> Sub.batch [ restart Restart, incomingResult IncomingResult ]
        }



-- Types


{-| Arguments passed to the reporter at startup,
such as the initial random seed, the number of fuzz runs,
and the type of reporter requested: (default)Console|Json|Junit
-}
type alias Flags =
    { initialSeed : Int
    , fuzzRuns : Int
    , mode : String
    }


{-| Main model. Exposed for usage in type definitions.
-}
type alias Model =
    { ports : Ports Msg
    , reporter : Interface
    , testsCount : Int
    , testResults : Array TestResult
    , kind : Result String SeededRunners.Kind
    }


{-| Internal messages.
-}
type Msg
    = Restart { kind : String, testsCount : Int }
    | IncomingResult { duration : Float, result : Value, logs : List String }
    | Summarize
    | Finished



-- Functions


chooseReporter : Flags -> Interface
chooseReporter { initialSeed, fuzzRuns, mode } =
    case mode of
        "json" ->
            ReporterJson.implementation { seed = initialSeed, fuzzRuns = fuzzRuns }

        "junit" ->
            ReporterJunit.implementation

        _ ->
            -- ReporterConsole.implementation { seed = initialSeed, fuzzRuns = fuzzRuns }
            ReporterConsoleBis.implementation { seed = initialSeed, fuzzRuns = fuzzRuns }


init : Ports Msg -> Flags -> ( Model, Cmd Msg )
init ports flags =
    let
        reporter =
            chooseReporter flags
    in
    ( Model ports reporter 0 Array.empty (Ok SeededRunners.Plain), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart { kind, testsCount } ->
            if String.startsWith "Invalid" kind then
                ( { model | kind = Err (String.dropLeft 7 kind) }, delayedMsg Summarize )

            else if testsCount == 0 then
                ( { model | kind = kindFromString kind }, delayedMsg Summarize )

            else
                ( { model | kind = kindFromString kind, testsCount = testsCount }
                , report model.ports.stdout (model.reporter.onBegin testsCount)
                )

        IncomingResult { duration, result, logs } ->
            let
                testResultResult =
                    decodeValue TestResult.decoder result
                        |> Result.map (TestResult.setLogs logs >> TestResult.setDuration duration)

                allTestResults =
                    case testResultResult of
                        Ok testResult ->
                            Array.push testResult model.testResults

                        Err _ ->
                            model.testResults

                updatedModel =
                    { model | testResults = allTestResults }
            in
            if Array.length updatedModel.testResults == model.testsCount then
                ( updatedModel
                , Result.map model.reporter.onResult testResultResult
                    |> Result.map (reportAndThenSummarize model.ports.stdout)
                    |> Result.withDefault Cmd.none
                )

            else
                ( updatedModel
                , Result.map model.reporter.onResult testResultResult
                    |> Result.map (report model.ports.stdout)
                    |> Result.withDefault Cmd.none
                )

        Summarize ->
            ( model, summarize model.ports.stdout (model.reporter.onEnd model.kind model.testResults) )

        Finished ->
            ( model
            , model.ports.signalFinished
                { exitCode = errorCode model.kind model.testResults
                , testsCount = model.testsCount
                }
            )


errorCode : Result String SeededRunners.Kind -> Array TestResult -> Int
errorCode kindResult testResults =
    let
        { failedCount, todoCount } =
            TestResult.summary testResults
    in
    if kindResult == Ok SeededRunners.Plain && failedCount + todoCount == 0 then
        0

    else
        2


reportAndThenSummarize : (String -> Cmd Msg) -> Maybe String -> Cmd Msg
reportAndThenSummarize stdout content =
    Cmd.batch [ report stdout content, delayedMsg Summarize ]


report : (String -> Cmd Msg) -> Maybe String -> Cmd Msg
report stdout content =
    Maybe.map stdout content
        |> Maybe.withDefault Cmd.none


summarize : (String -> Cmd Msg) -> Maybe String -> Cmd Msg
summarize stdout content =
    case content of
        Just string ->
            Cmd.batch [ stdout string, delayedMsg Finished ]

        Nothing ->
            delayedMsg Finished


delayedMsg : msg -> Cmd msg
delayedMsg msg =
    Process.sleep 0
        |> Task.andThen (\_ -> Task.succeed msg)
        |> Task.perform identity
