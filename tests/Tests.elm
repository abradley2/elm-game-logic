module Tests exposing (..)

import Expect
import Logic.Component
import Logic.Entity
import Logic.System
import Test exposing (Test, describe, test)


systemTest : Test
systemTest =
    describe "Advanced Folds" <|
        let
            initWorld =
                { foos = Logic.Component.empty
                , bars = Logic.Component.empty
                }

            withEntities =
                List.indexedMap (\n comps -> List.map (\c -> ( n, c )) comps)
                    >> List.concat
                    >> List.foldl
                        (\( idx, comp ) w -> Logic.Entity.with comp ( idx, w ) |> Tuple.second)
                        initWorld

            fooSpec =
                Logic.Component.Spec
                    .foos
                    (\foos w -> { w | foos = foos })

            barSpec =
                Logic.Component.Spec
                    .bars
                    (\bars w -> { w | bars = bars })
        in
        [ test "advanced fold all required" <|
            \_ ->
                let
                    world =
                        withEntities
                            [ [ ( fooSpec, 1 ), ( barSpec, 2 ) ]
                            ]

                    result =
                        Logic.System.advancedFoldl2
                            (\_ a b acc -> acc + a + b)
                            (Logic.System.isRequired world.foos)
                            (Logic.System.isRequired world.bars)
                            0
                in
                Expect.equal result 3
        , test "advanced fold all required 2" <|
            \_ ->
                let
                    world =
                        withEntities
                            [ [ ( barSpec, 2 ) ]
                            , [ ( fooSpec, 2 ) ]
                            , [ ( fooSpec, 4 ), ( barSpec, 5 ) ]
                            ]

                    result =
                        Logic.System.advancedFoldl2
                            (\_ a b acc -> acc + a + b)
                            (Logic.System.isRequired world.foos)
                            (Logic.System.isRequired world.bars)
                            0
                in
                Expect.equal result 9
        , test "advanced fold with optionals" <|
            \_ ->
                let
                    world =
                        withEntities
                            [ -- entity 1
                              [ ( fooSpec, 1 ) ]

                            -- entity 2
                            , [ ( fooSpec, 2 ), ( barSpec, 3 ) ]

                            -- entity 3
                            , [ ( barSpec, 999 ) ]
                            ]

                    result =
                        Logic.System.advancedFoldl2
                            (\_ a b acc -> acc + a + Maybe.withDefault 0 b)
                            (Logic.System.isRequired world.foos)
                            (Logic.System.isOptional world.bars)
                            0
                in
                Expect.equal result 6
        ]
