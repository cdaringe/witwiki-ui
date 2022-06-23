module Api.Endpoint exposing (Endpoint, article, comment, comments, favorite, feed, follow, login, posts, profiles, request, tags, user, users)

import CommentId exposing (CommentId)
import Http
import Post.Slug as Slug exposing (Slug)
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , withCredentials : Bool
    }
    -> Http.Request a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , withCredentials = config.withCredentials
        }



-- TYPES


{-| Get a URL to the Conduit API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


type Origin
    = Conduit
    | Wit


url : List String -> List QueryParameter -> Origin -> Endpoint
url paths queryParams origin =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin
        (case origin of
            Conduit ->
                "https://conduit.productionready.io"

            Wit ->
                "http://localhost:3000"
        )
        ("api" :: paths)
        queryParams
        |> Endpoint



-- ENDPOINTS


login : Endpoint
login =
    url [ "users", "login" ] [] Conduit


user : Endpoint
user =
    url [ "user" ] [] Conduit


users : Endpoint
users =
    url [ "users" ] [] Conduit


follow : Username -> Endpoint
follow uname =
    url [ "profiles", Username.toString uname, "follow" ] [] Conduit



-- ARTICLE ENDPOINTS


article : Slug -> Endpoint
article slug =
    url [ "posts", Slug.toString slug ] [] Conduit


comments : Slug -> Endpoint
comments slug =
    url [ "posts", Slug.toString slug, "comments" ] [] Conduit


comment : Slug -> CommentId -> Endpoint
comment slug commentId =
    url [ "posts", Slug.toString slug, "comments", CommentId.toString commentId ] [] Conduit


favorite : Slug -> Endpoint
favorite slug =
    url [ "posts", Slug.toString slug, "favorite" ] [] Conduit


posts : List QueryParameter -> Endpoint
posts params =
    url [ "posts", "recent" ] params Wit


profiles : Username -> Endpoint
profiles uname =
    url [ "profiles", Username.toString uname ] [] Conduit


feed : List QueryParameter -> Endpoint
feed params =
    url [ "posts", "feed" ] params Conduit


tags : Endpoint
tags =
    url [ "tags" ] [] Conduit
