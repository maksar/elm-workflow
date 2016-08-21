

module Tests exposing (all)

import Function.Extra exposing (twice)
import Check exposing (..)
import Check.Test exposing (evidenceToTest)
import Check.Producer exposing (tuple, list)
import List exposing (foldl)
import ElmTest exposing (Test)
import Permission exposing (..)
import User exposing (..)
import Workflow exposing (..)
import Producers exposing (..)


realWorldWorkflow : Workflow
realWorldWorkflow =
    [ 3, 2, 2 ]
        |> Workflow.init
        |> approve (create "User 1" True [ VOTE ])
        |> approve (create "User 1" True [ VOTE ])
        |> approve (create "User 2" True [ VOTE ])
        |> approve (create "User 3" False [ VOTE ])
        |> approve (create "User 4" True [ VOTE ])
        |> approve (create "User 5" True [ NONE, FORCE ])
        |> reject (create "User 6" True [ NONE, NONE, VOTE ])
        |> approve (create "User 7" True [ NONE, VOTE ])
        |> approve (create "User 8" True [ NONE, VOTE ])
        |> approve (create "User 9" True [ NONE, VOTE, FORCE ])


initWorkflow : Workflow
initWorkflow =
    init [ 1, 2, 3 ]


workflowVotedByUser : User -> Workflow
workflowVotedByUser user =
    let
        user =
            create (name user) True [ VOTE, VOTE ]

    in
        initWorkflow |> twice (approve user)

test : Test
test = ElmTest.test "Real world Workflow" <| ElmTest.assertEqual True (finished realWorldWorkflow)

claims : Claim
claims =
    Check.suite "Workflow rules"
        [ claim "Same user cannot vote twice"
            `that` (\( user, operation ) -> (workflowVotedByUser user |> apply operation user).currentStep)
            `is` (\( user, operation ) -> (workflowVotedByUser user).currentStep)
            `for` tuple ( userProducer, operationProducer )
        , claim "Workflow should remain in its bounds"
            `true`
                (\users ->
                    let
                        step =
                            (foldl approve initWorkflow users).currentStep
                    in
                        step >= 0 && step <= 4
                )
            `for` list userProducer
        ]


all : Test
all =
    ElmTest.suite "A Test Suite"
        [ evidenceToTest (quickCheck claims), test ]
