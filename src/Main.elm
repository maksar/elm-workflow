module Main exposing (..)

import Workflow exposing (..)
import Permission exposing (Permission(..))
import User exposing (User, create, name)
import Html exposing (Html, text)
import Dict exposing (Dict, insert, empty)
import GenericSet exposing (GenericSet)
import Array exposing (Array)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import TimeTravel.Html.App as TimeTravel



type alias Model =
    { users : List User, workflow : Workflow }


type Msg
    = Approve User
    | Reject User
    | None


color : User -> String
color user =
    Maybe.withDefault "red" <|
        Dict.get (name user)
            (empty
                |> insert "User 1" "lightskyblue"
                |> insert "User 2" "green"
                |> insert "User 3" "darkgray"
                |> insert "User 4" "purple"
                |> insert "User 5" "red"
                |> insert "User 6" "aqua"
                |> insert "User 7" "lightgray"
                |> insert "User 8" "yellow"
                |> insert "User 9" "red"
            )


init : ( Model, Cmd Msg )
init =
    ( { users =
            [ (create "User 1" True [ VOTE ])
            , (create "User 2" True [ VOTE ])
            , (create "User 3" False [ VOTE ])
            , (create "User 4" True [ VOTE ])
            , (create "User 5" True [ NONE, FORCE ])
            , (create "User 6" True [ NONE, VOTE ])
            , (create "User 7" True [ NONE, VOTE ])
            , (create "User 8" True [ NONE, NONE, VOTE ])
            , (create "User 9" True [ NONE, VOTE, FORCE ])
            ]
      , workflow = Workflow.init [ 3, 2, 2 ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel user action =
            { model | workflow = action user model.workflow }
    in
        case msg of
            Approve user ->
                ( newModel user Workflow.approve, Cmd.none )

            Reject user ->
                ( newModel user Workflow.reject, Cmd.none )

            None ->
                ( model, Cmd.none )


viewUser : Model -> Int -> User -> Html Msg
viewUser model index user =
    let
        button caption position action =
            Html.a
                [ style
                    [ ( "border", "1px solid white" )
                    , ( "display", "inline-block" )
                    , ( "float", position )
                    , ( "text-align", "center" )
                    , ( "width", "20px" )
                    , ( "height", "20px" )
                    ]
                , onClick <| action user
                ]
                [ text caption ]
    in
        Html.div
            [ style
                [ ( "width", "50px" )
                , ( "height", "50px" )
                , ( "display", "inline-block" )
                , ( "margin-right", "10px" )
                , ( "background-color", color user )
                ]
            ]
            [ text <| (name user)
            , button "-" "left" Reject
            , button "+" "right" Approve
            ]


viewVotes : Model -> GenericSet User -> Html Msg
viewVotes model votes =
    let
        box user =
            Html.div
                [ style
                    [ ( "width", "100%" )
                    , ( "height", "20px" )
                    , ( "margin-bottom", "10px" )
                    , ( "border", "solid 1px green" )
                    , ( "background-color", color user )
                    , ( "opacity", "1" )
                    ]
                ]
                [ text <| toString <| (name user) ]
    in
        Html.div [] (List.map box (GenericSet.toList votes))


viewStep : Model -> Int -> GenericSet User -> Html Msg
viewStep model index votes =
    Html.div
        [ style
            [ ( "width", "200px" )
            , ( "margin-right", "100px" )
            , ( "float", "left" )
            , ( "display", "inline-block" )
            ]
        ]
        [ Html.div
            [ style
                [ ( "width", "100%" )
                , ( "height", "100px" )
                , ( "margin-bottom", "10px" )
                , ( "border", "solid 1px black" )
                , ( "background-color"
                  , if model.workflow.currentStep > index then
                        "green"
                    else
                        "white"
                  )
                ]
            ]
            [ text <| toString <| "step " ++ (toString index) ++ " requires " ++ (toString <| Array.length model.workflow.stepsConfig) ++ " approves" ]
        , viewVotes model votes
        ]


view : Model -> Html Msg
view model =
    Html.div [] <|
        List.concat
            [ List.indexedMap (viewUser model) model.users
            , [ Html.div [ style [ ( "height", "50px" ) ] ] [] ]
            , Array.toList <| Array.indexedMap (viewStep model) model.workflow.votes
            , [ Html.div [ style [ ( "height", "50px" ) ] ] [] ]
            , [ model.workflow |> toString |> text ]
            , [ Html.div [ style [ ( "height", "50px" ) ] ] [] ]
            , [ (finished model.workflow) |> toString |> text ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    TimeTravel.program
        { init = init, view = view, update = update, subscriptions = subscriptions }
