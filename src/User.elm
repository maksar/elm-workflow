module User
    exposing
        ( User
        , create
        , permission
        , compare
        , name
        , active
        , permissions
        )

{-| User type, instances of which should be used in `Workflow`s

# Definition
@docs User

# Creating User
@docs create

# Taking User Apart
@docs name, active, permissions

# Getting Permission for a step
@docs permission

# Comparing Users
@docs compare
-}

import Array exposing (Array)
import Permission exposing (Permission, Permission(..))

{-| Container for all attributes, required by `Workflow` to operate. -}
type User
    = User { name : String, active : Bool, permissions : Array Permission }


{-| Creates instance of the `User` by name, operational status, and list of Permissions for each step.

    create "Bob" True [ NONE, VOTE ] == User "Bob" True Array.fromList [ NONE, VOTE ]
-}
create : String -> Bool -> List (Permission) -> User
create name active permissions =
    User { name = name, active = active, permissions = (Array.fromList permissions) }


{-| Gets permission from a user to a particular step of the `Workflow`.
    If permission wasn't found, `NONE` will be returned.

    permission 0 <| create "Bob" True [ NONE, VOTE ] == NONE
    permission 1 <| create "Bob" True [ NONE, VOTE ] == VOTE
    permission 2 <| create "Bob" True [ NONE, VOTE ] == NONE
-}
permission : Int -> User -> Permission
permission index (User { permissions }) =
    Array.get index permissions
        |> Maybe.withDefault NONE


{-| Compare names of the two given users, returning `Order`. No other attributes are taken into account.

    compare (create "Bob" True [ NONE ]) (create "Alice" True [ NONE ]) == False
    compare (create "Bob" True [ NONE ]) (create "Bob" False [ ]) == True
-}
compare : User -> User -> Order
compare (User user1) (User user2) =
    Basics.compare (user1.name) (user2.name)


{-| Extracts name from `User` instance.

    name <| create "Bob" True [ NONE, VOTE ] == "Bob"
-}
name : User -> String
name (User { name }) =
    name


{-| Extracts operational status from `User` instance.

    active <| create "Bob" True [ NONE, VOTE ] == True
-}
active : User -> Bool
active (User { active }) =
    active


{-| Extracts list of `Permission`s from `User` instance.

    permissions <| create "Bob" True [ NONE, VOTE ] == [ NONE, VOTE ]
-}
permissions : User -> List Permission
permissions (User { permissions }) =
    Array.toList permissions
