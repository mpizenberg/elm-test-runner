module ElmTestRunner.Runner exposing
    ( worker, Ports
    , Flags, Model, Msg
    )

{-| Main module for a test runner worker.


# Create a main test runner worker

@docs worker, Ports


# Internal types for function signatures

@docs Flags, Model, Msg

-}

import Array
import ElmTestRunner.Result as TestResult exposing (TestResult)
import ElmTestRunner.SeededRunners as SeededRunners exposing (SeededRunners)
import Json.Encode exposing (Value)
import Platform
import Test exposing (Test)



-- Ports


{-| Ports(ish) required by the worker program to function.
They aren't exactly ports (`sendResult` isn't a valid port for example)
but will basically be wrapped by an actual port in the main Elm caller module.
-}
type alias Ports msg =
    { askTestsCount : (Value -> msg) -> Sub msg
    , sendTestsCount : Int -> Cmd msg
    , receiveRunTest : ({ id : Int, startTime : Float } -> msg) -> Sub msg
    , sendResult : { id : Int, startTime : Float } -> Value -> Cmd msg
    }



-- Types


{-| The initial random seed and the number of fuzz runs are passed as flags.
-}
type alias Flags =
    { initialSeed : Int
    , fuzzRuns : Int
    }


{-| Main model. Exposed for usage in type definitions.
-}
type alias Model =
    { ports : Ports Msg
    , testRunners : SeededRunners
    }


{-| Internal messages.
-}
type Msg
    = AskTestsCount
    | ReceiveRunTest { id : Int, startTime : Float }



-- Functions


{-| Create a test runner worker.
Some specific ports(ish) are required as arguments,
as well as the "master" test concatenating all tests (CF `SeededRunners.fromTest`).

The main Elm module calling this one will typically look like the example below.
In that code, {{ user\_imports }} and {{ tests }} are to be replaced by the list
of needed imports from user code and the list of tests to run.

    port module Runner exposing (main)

    {{ user_imports }}
    import Test
    import ElmTestRunner.Runner exposing (Flags, Model, Msg)
    import Json.Encode exposing (Value)

    port askTestsCount : (Value -> msg) -> Sub msg
    port sendTestsCount : { type_ : String, testsCount : Int } -> Cmd msg
    port receiveRunTest : ({ id : Int, startTime : Float }  -> msg) -> Sub msg
    port sendResult : { type_ : String, id: Int, startTime : Float, result : Value } -> Cmd msg

    main : Program Flags Model Msg
    main =
        [ {{ tests }} ]
            |> Test.concat
            |> ElmTestRunner.Runner.worker
                { askTestsCount = askTestsCount
                , sendTestsCount = \count -> sendTestsCount { type_ = "testsCount", testsCount = count }
                , receiveRunTest = receiveRunTest
                , sendResult = \id res -> sendResult { type_ = "result", id = id, result = res }
                }

It can later be spawned as a Node worker with a tiny bit of JS similar to:

```js
const { parentPort } = require("worker_threads");
const { Elm } = require("./Runner.elm.js");

// Start the Elm app
const flags = { initialSeed: ..., fuzzRuns: ... };
const app = Elm.Runner.init({ flags: flags });

// Communication between Elm runner worker and Supervisor via port
app.ports.outgoing.subscribe((msg) => parentPort.postMessage(msg));
parentPort.on("message", (msg) => app.ports.incoming.send(msg));
```

-}
worker : Ports Msg -> Maybe Test -> Program Flags Model Msg
worker ({ askTestsCount, receiveRunTest } as ports) masterTest =
    Platform.worker
        { init = init masterTest ports
        , update = update
        , subscriptions = \_ -> Sub.batch [ askTestsCount (always AskTestsCount), receiveRunTest ReceiveRunTest ]
        }


init : Maybe Test -> Ports Msg -> Flags -> ( Model, Cmd Msg )
init maybeConcatenatedTest ports flags =
    case maybeConcatenatedTest of
        Nothing ->
            ( Model ports SeededRunners.empty, Cmd.none )

        Just concatenatedTest ->
            ( Model ports (SeededRunners.fromTest concatenatedTest flags), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.testRunners ) of
        -- AskTestsCount
        ( AskTestsCount, Ok { runners } ) ->
            ( model, model.ports.sendTestsCount (Array.length runners) )

        ( AskTestsCount, Err _ ) ->
            ( model, Debug.todo "Deal with invalid runners" )

        -- ReceiveRunTest
        ( ReceiveRunTest meta, Ok { runners } ) ->
            ( model, sendTestResult model.ports meta (SeededRunners.run meta.id runners) )

        ( ReceiveRunTest _, Err _ ) ->
            ( model, Debug.todo "Deal with invalid runners" )


sendTestResult : Ports msg -> { id : Int, startTime : Float } -> Maybe TestResult -> Cmd msg
sendTestResult ports meta maybeResult =
    Maybe.map TestResult.encode maybeResult
        |> Maybe.map (ports.sendResult meta)
        |> Maybe.withDefault Cmd.none
