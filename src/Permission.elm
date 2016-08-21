module Permission exposing (..)

{-| Permission type dictating what `User` can do on particular `Workflow` step.

# Definition
@docs Permission

-}


{-| User can vote (having `VOTE` permission) on particular step of the `Workflow` or completely skip the whole step
    (having `FORCE` permission). Without any permissions (`NONE`), `User` cannot affect the `Workflow`.
-}
type Permission
    = VOTE
    | FORCE
    | NONE
