module Tests exposing (claims, traditionalTest)

import Array exposing (length)
import Function.Extra exposing (twice)
import Check exposing (Claim, claim)
import Check.Producer exposing (tuple, list)
import List exposing (foldl, range, member)
import Test exposing (Test, describe, test)
import Expect exposing (equal)
import CheckCompat exposing (..)
import Permission exposing (..)
import User exposing (..)
import Workflow exposing (..)
import Producers exposing (..)


initWorkflow : Workflow
initWorkflow =
    init [ 1, 2, 3 ]


workflowVotedByUser : User -> Workflow
workflowVotedByUser user =
    let
        userWithSameName =
            create (name user) True [ VOTE, VOTE ]
    in
        initWorkflow |> twice (approve userWithSameName)


traditionalTest : Test
traditionalTest =
    let
        workflow =
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

        finalUser =
            create "User 9" True [ NONE, VOTE, FORCE ]
    in
        describe "Real world workflow example"
            [ test "Initially, workflow is not finished" <| \() -> equal False (finished workflow)
            , test "But after approve it becomes finished" <| \() -> equal True (finished (approve finalUser workflow))
            ]


claims : Claim
claims =
    Check.suite "Workflow rules"
        [ claim "Same user cannot vote twice"
            |> that (\( user, operation ) -> (workflowVotedByUser user |> apply operation user).currentStep)
            |> is (\( user, operation ) -> (workflowVotedByUser user).currentStep)
            |> for (tuple ( userProducer, operationProducer ))
        , claim "Workflow should remain in its bounds"
            |> true
                (\users ->
                    member (foldl approve initWorkflow users).currentStep <|
                        range 0 (length initWorkflow.stepsConfig)
                )
            |> for (list userProducer)
        ]
