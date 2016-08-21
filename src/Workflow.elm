module Workflow
    exposing
        ( Workflow
        , init
        , approve
        , reject
        , finished
        )

{-| Workflow simulates voting process on multiple steps.
    Each step has a threshold value.
    To proceed to the next step, it's necessary to get threshold number of votes.
    Power users can force skip one step.
    User with ar least `VOTE` permission can `reject` the curren step (previous step re-starts).
    Inactive users canot `approve` or `reject`. User can only vote once on the same step.

# Definition
@docs Workflow

# Creating Workflow
@docs init

# Interactions
@docs approve, reject

# Inspecting state
@docs finished
-}

import Array exposing (Array, length, repeat, fromList, get, set)
import Array.Extra exposing (update)
import Maybe exposing (withDefault)
import Random exposing (maxInt)
import User exposing (User)
import Permission exposing (Permission(..))
import GenericSet exposing (GenericSet)


{-| Record representing workflow status. Consists of:
    `stepsConfig` - array, containing step's threshold values;
    `currentStep` integer - number of the current workflow step;
    `votes` - array of votes on each step.
-}
type alias Workflow =
    { stepsConfig : Array Int
    , currentStep : Int
    , votes : Array (GenericSet User)
    }


emptySet : GenericSet User
emptySet =
    GenericSet.empty User.compare


{-| Creates `Workflow` from provided list of threshold values.

    init [ 1, 2 ] == { stepsConfig = Array.fromList [1,2], currentStep = 0, votes = Array.fromList [Set.fromList [],Set.fromList []] }
-}
init : List Int -> Workflow
init steps =
    let
        config =
            fromList steps
    in
        { stepsConfig = config
        , currentStep = 0
        , votes = repeat (length config) emptySet
        }


{-| Performs approve (vote) operation from given `User` on a `Workflow`.

    init [ 1, 2 ] |> approve (create "Bob" True [VOTE]) == { stepsConfig = Array.fromList [1,2], currentStep = 1, votes = Array.fromList [Set.fromList [User { name = "Bob", active = True, permissions = Array.fromList [VOTE] }],Set.fromList []] }
-}
approve : User -> Workflow -> Workflow
approve user workflow =
    if locked user workflow then
        workflow
    else
        case User.permission workflow.currentStep user of
            FORCE ->
                increment user workflow

            VOTE ->
                vote user workflow

            NONE ->
                workflow


{-| Performs reject operation from given `User` on a `Workflow`.
-}
reject : User -> Workflow -> Workflow
reject user workflow =
    if locked user workflow then
        workflow
    else
        case User.permission workflow.currentStep user of
            FORCE ->
                decrement workflow

            VOTE ->
                decrement workflow

            NONE ->
                workflow


increment : User -> Workflow -> Workflow
increment user workflow =
    let
        newWorkflow =
            { workflow | votes = update workflow.currentStep (\bucket -> GenericSet.insert user bucket) workflow.votes }
    in
        step Forward newWorkflow


decrement : Workflow -> Workflow
decrement workflow =
    let
        clean workflow =
            { workflow | votes = set workflow.currentStep emptySet workflow.votes }
    in
        if workflow.currentStep == 0 then
            workflow |> clean
        else
            workflow
                |> clean
                |> step Backward
                |> clean


locked : User -> Workflow -> Bool
locked user workflow =
    let
        votes =
            withDefault emptySet (get workflow.currentStep workflow.votes)

        alreadyVoted user =
            GenericSet.member user votes
    in
        if finished workflow then
            True
        else if not (User.active user) then
            True
        else if alreadyVoted user then
            True
        else
            False


vote : User -> Workflow -> Workflow
vote user workflow =
    let
        newVotes =
            update workflow.currentStep (\bucket -> GenericSet.insert user bucket) workflow.votes

        newWorkflow =
            { workflow | votes = newVotes }

        requiredVotes =
            withDefault maxInt (get newWorkflow.currentStep newWorkflow.stepsConfig)

        votesCount =
            GenericSet.size <| withDefault emptySet (get newWorkflow.currentStep newWorkflow.votes)
    in
        if votesCount >= requiredVotes then
            newWorkflow |> increment user
        else
            newWorkflow


{-| Returns high-level `Workflow` completion status. `Workflow` becomes completed after going through all its steps.
-}
finished : Workflow -> Bool
finished workflow =
    workflow.currentStep == length workflow.stepsConfig


type Direction
    = Forward
    | Backward


step : Direction -> Workflow -> Workflow
step direction workflow =
    let
        count =
            case direction of
                Forward ->
                    1

                Backward ->
                    -1
    in
        { workflow | currentStep = workflow.currentStep + count }
