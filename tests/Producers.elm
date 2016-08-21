module Producers exposing (..)

import Check.Producer exposing (..)
import Workflow exposing (..)
import User exposing (..)
import Function.Extra exposing (uncurry3)
import Permission exposing (..)


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
