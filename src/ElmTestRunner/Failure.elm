module ElmTestRunner.Failure exposing (Failure, decoder, encode)

{-| Encode and decode test failures.

@docs Failure, decoder, encode

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| Type resulting of calling `Test.Runner.getFailureReason` on a test `Expectation`.
-}
type alias Failure =
    { given : Maybe String
    , description : String
    , reason : Reason
    }


{-| Decode a `Failure`.
-}
decoder : Decoder Failure
decoder =
    decodeFailure


{-| Encode a `Failure`.
-}
encode : Failure -> Value
encode =
    encodeFailure



-- Generated decoders and encoders thanks to (https://dkodaj.github.io/decgen/)
--
-- type Reason
--     = Custom
--     | Equality String String
--     | Comparison String String
--     | ListDiff (List String) (List String)
--     | CollectionDiff
--         { expected : String
--         , actual : String
--         , extra : List String
--         , missing : List String
--         }
--     | TODO
--     | Invalid InvalidReason
--
-- type InvalidReason
--     = EmptyList
--     | NonpositiveFuzzCount
--     | InvalidFuzzer
--     | BadDescription
--     | DuplicatedName
--     | CoverageInsufficient
--     | CoverageBug


type alias Record_expected_String_actual_String_extra_ListString_missing_ListString_ =
    { expected : String, actual : String, extra : List String, missing : List String }


decodeFailure : Decoder Failure
decodeFailure =
    Decode.map3
        Failure
        (Decode.field "given" (Decode.maybe Decode.string))
        (Decode.field "description" Decode.string)
        (Decode.field "reason" decodeReason)


decodeInvalidReason : Decoder InvalidReason
decodeInvalidReason =
    let
        recover x =
            case x of
                "EmptyList" ->
                    Decode.succeed EmptyList

                "NonpositiveFuzzCount" ->
                    Decode.succeed NonpositiveFuzzCount

                "InvalidFuzzer" ->
                    Decode.succeed InvalidFuzzer

                "BadDescription" ->
                    Decode.succeed BadDescription

                "DuplicatedName" ->
                    Decode.succeed DuplicatedName

                "CoverageInsufficient" ->
                    Decode.succeed CoverageInsufficient

                "CoverageBug" ->
                    Decode.succeed CoverageBug

                other ->
                    Decode.fail <| "Unknown constructor for type InvalidReason: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeReason : Decoder Reason
decodeReason =
    Decode.field "Constructor" Decode.string |> Decode.andThen decodeReasonHelp


decodeReasonHelp constructor =
    case constructor of
        "Custom" ->
            Decode.succeed Custom

        "Equality" ->
            Decode.map2
                Equality
                (Decode.field "A1" Decode.string)
                (Decode.field "A2" Decode.string)

        "Comparison" ->
            Decode.map2
                Comparison
                (Decode.field "A1" Decode.string)
                (Decode.field "A2" Decode.string)

        "ListDiff" ->
            Decode.map2
                ListDiff
                (Decode.field "A1" (Decode.list Decode.string))
                (Decode.field "A2" (Decode.list Decode.string))

        "CollectionDiff" ->
            Decode.map
                CollectionDiff
                (Decode.field "A1" decodeRecord_expected_String_actual_String_extra_ListString_missing_ListString_)

        "TODO" ->
            Decode.succeed TODO

        "Invalid" ->
            Decode.map
                Invalid
                (Decode.field "A1" decodeInvalidReason)

        other ->
            Decode.fail <| "Unknown constructor for type Reason: " ++ other


decodeRecord_expected_String_actual_String_extra_ListString_missing_ListString_ =
    Decode.map4
        Record_expected_String_actual_String_extra_ListString_missing_ListString_
        (Decode.field "expected" Decode.string)
        (Decode.field "actual" Decode.string)
        (Decode.field "extra" (Decode.list Decode.string))
        (Decode.field "missing" (Decode.list Decode.string))


encodeFailure : Failure -> Value
encodeFailure a =
    Encode.object
        [ ( "given", encodeMaybe Encode.string a.given )
        , ( "description", Encode.string a.description )
        , ( "reason", encodeReason a.reason )
        ]


encodeInvalidReason : InvalidReason -> Value
encodeInvalidReason a =
    Encode.string <|
        case a of
            EmptyList ->
                "EmptyList"

            NonpositiveFuzzCount ->
                "NonpositiveFuzzCount"

            InvalidFuzzer ->
                "InvalidFuzzer"

            BadDescription ->
                "BadDescription"

            DuplicatedName ->
                "DuplicatedName"

            CoverageInsufficient ->
                "CoverageInsufficient"

            CoverageBug ->
                "CoverageBug"


encodeMaybe f a =
    case a of
        Just b ->
            f b

        Nothing ->
            Encode.null


encodeReason : Reason -> Value
encodeReason a =
    case a of
        Custom ->
            Encode.object
                [ ( "Constructor", Encode.string "Custom" )
                ]

        Equality a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "Equality" )
                , ( "A1", Encode.string a1 )
                , ( "A2", Encode.string a2 )
                ]

        Comparison a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "Comparison" )
                , ( "A1", Encode.string a1 )
                , ( "A2", Encode.string a2 )
                ]

        ListDiff a1 a2 ->
            Encode.object
                [ ( "Constructor", Encode.string "ListDiff" )
                , ( "A1", Encode.list Encode.string a1 )
                , ( "A2", Encode.list Encode.string a2 )
                ]

        CollectionDiff a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "CollectionDiff" )
                , ( "A1", encodeRecord_expected_String_actual_String_extra_ListString_missing_ListString_ a1 )
                ]

        TODO ->
            Encode.object
                [ ( "Constructor", Encode.string "TODO" )
                ]

        Invalid a1 ->
            Encode.object
                [ ( "Constructor", Encode.string "Invalid" )
                , ( "A1", encodeInvalidReason a1 )
                ]


encodeRecord_expected_String_actual_String_extra_ListString_missing_ListString_ a =
    Encode.object
        [ ( "expected", Encode.string a.expected )
        , ( "actual", Encode.string a.actual )
        , ( "extra", Encode.list Encode.string a.extra )
        , ( "missing", Encode.list Encode.string a.missing )
        ]
