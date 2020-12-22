-- Copied from rtfeldman/node-test-runner : elm/src/Test/Reporter/Console/Format/Color.elm
-- https://github.com/rtfeldman/node-test-runner/blob/master/elm/src/Test/Reporter/Console/Format/Color.elm


module ElmTestRunner.Vendor.FormatColor exposing (formatEquality)

import ElmTestRunner.Vendor.Highlightable as Highlightable exposing (Highlightable(..))
import ElmTestRunner.Vendor.NodeConsole as Console


formatEquality : List (Highlightable String) -> List (Highlightable String) -> ( String, String )
formatEquality highlightedExpected highlightedActual =
    let
        formattedExpected =
            highlightedExpected
                |> List.map fromHighlightable
                |> String.join ""

        formattedActual =
            highlightedActual
                |> List.map fromHighlightable
                |> String.join ""
    in
    ( formattedExpected, formattedActual )


fromHighlightable : Highlightable String -> String
fromHighlightable =
    Highlightable.resolve
        -- Cyan seems to look readable with both white and black text on top,
        -- so it should work with both dark and light console themes
        { fromHighlighted = Console.colorsInverted
        , fromPlain = identity
        }
