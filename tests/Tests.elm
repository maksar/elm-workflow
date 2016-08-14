--test : Workflow String
--test =
--    [ 3, 2, 2 ]
--        |> Workflow.init
--        |> approve (create "User 1" True [ VOTE ])
--        |> approve (create "User 1" True [ VOTE ])
--        |> approve (create "User 2" True [ VOTE ])
--        |> approve (create "User 3" False [ VOTE ])
--        |> approve (create "User 4" True [ VOTE ])
--        |> approve (create "User 5" True [ NONE, FORCE ])
--        |> reject (create "User 6" True [ NONE, NONE, VOTE ])
--        |> approve (create "User 7" True [ NONE, VOTE ])
--        |> approve (create "User 8" True [ NONE, VOTE ])
--        |> approve (create "User 9" True [ NONE, VOTE, FORCE ])


module Tests exposing (all)

import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test exposing (..)
import List exposing (..)
import Workflow exposing (..)
import ElmTest
import User exposing (..)
import Permission exposing (..)


uncurry3 : (a -> b -> c -> x) -> ( a, b, c ) -> x
uncurry3 f ( a, b, c ) =
    f a b c


permissionProducer : Producer Permission
permissionProducer =
    let
        permission i =
            case i of
                1 ->
                    VOTE

                2 ->
                    FORCE

                _ ->
                    NONE

        unPermission permission =
            case permission of
                VOTE ->
                    1

                FORCE ->
                    2

                NONE ->
                    0
    in
        convert permission unPermission <| rangeInt 0 2


toTuple : User -> ( String, Bool, List Permission )
toTuple user =
    ( (name user), (active user), (permissions user) )


userProducer : Producer User
userProducer =
    convert (uncurry3 create)
        toTuple
        (tuple3 ( string, bool, list permissionProducer ))


type Operation
    = Approve
    | Reject


apply : Operation -> User -> Workflow -> Workflow
apply operation user workflow =
    case operation of
        Approve ->
            approve user workflow

        Reject ->
            reject user workflow


operationProducer : Producer Operation
operationProducer =
    convert
        (\b ->
            if b then
                Approve
            else
                Reject
        )
        (\operation ->
            case operation of
                Approve ->
                    True

                Reject ->
                    False
        )
        bool


myClaims : Claim
myClaims =
    Check.suite "Workflow rules"
        [ claim "Same user cannot vote twice"
            `true`
                (\( user, operation ) ->
                    let
                        existingUser =
                            create (name user) True [ VOTE, VOTE ]

                        existingWorkflow =
                            init [ 1, 2, 1 ] |> approve existingUser |> approve existingUser

                        currentStep =
                            case operation of
                                Approve ->
                                    (approve user existingWorkflow).currentStep

                                Reject ->
                                    (reject user existingWorkflow).currentStep
                    in
                        currentStep == existingWorkflow.currentStep
                )
            `for` tuple ( userProducer, operationProducer )
        , claim
            "Workflow should remain in its bounds"
            `true`
                (\users ->
                    let
                        currentStep =
                            (foldl approve (init [ 3, 2, 2 ]) users).currentStep
                    in
                        currentStep >= 0 && currentStep <= 4
                )
            `for` list userProducer
        ]


all : ElmTest.Test
all =
    ElmTest.suite "A Test Suite"
        [ evidenceToTest (quickCheck myClaims) ]
