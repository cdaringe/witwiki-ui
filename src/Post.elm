module Post exposing (Full, Post, Preview, author, body, favorite, favoriteButton, fetch, fromPreview, fullDecoder, post, previewDecoder, slug, unfavorite, unfavoriteButton)

{-| The interface to the Article data structure.

This includes:

  - The Article type itself
  - Ways to make HTTP requests to retrieve and modify articles
  - Ways to access information about an article
  - Converting between various types

-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Author exposing (Author(..))
import Html exposing (Attribute, Html, i)
import Html.Attributes exposing (class)
import Html.Events exposing (stopPropagationOn)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Post.Body as Body exposing (Body)
import Post.Slug as Slug exposing (Slug)
import Post.Tag as Tag exposing (Tag)
import Profile exposing (Profile)
import Time
import Username as Username exposing (Username)
import Viewer exposing (Viewer)



-- TYPES


{-| An article, optionally with an article body.

To see the difference between { extraInfo : a } and { extraInfo : Maybe Body },
consider the difference between the "view individual article" page (which
renders one article, including its body) and the "article feed" -
which displays multiple articles, but without bodies.

This definition for `Article` means we can write:

viewArticle : Article Full -> Html msg
viewFeed : List (Article Preview) -> Html msg

This indicates that `viewArticle` requires an article _with a `body` present_,
wereas `viewFeed` accepts articles with no bodies. (We could also have written
it as `List (Article a)` to specify that feeds can accept either articles that
have `body` present or not. Either work, given that feeds do not attempt to
read the `body` field from articles.)

This is an important distinction, because in Request.Article, the `feed`
function produces `List (Article Preview)` because the API does not return bodies.
Those articles are useful to the feed, but not to the individual article view.

-}
type Post a
    = Post Internals a


{-| Metadata about the article - its title, description, and so on.

Importantly, this module's public API exposes a way to read this metadata, but
not to alter it. This is read-only information!

If we find ourselves using any particular piece of metadata often,
for example `title`, we could expose a convenience function like this:

Post.title : Article a -> String

If you like, it's totally reasonable to expose a function like that for every one
of these fields!

(Okay, to be completely honest, exposing one function per field is how I prefer
to do it, and that's how I originally wrote this module. However, I'm aware that
this code base has become a common reference point for beginners, and I think it
is _extremely important_ that slapping some "getters and setters" on a record
does not become a habit for anyone who is getting started with Elm. The whole
point of making the Article type opaque is to create guarantees through
_selectively choosing boundaries_ around it. If you aren't selective about
where those boundaries are, and instead expose a "getter and setter" for every
field in the record, the result is an API with no more guarantees than if you'd
exposed the entire record directly! It is so important to me that beginners not
fall into the terrible "getters and setters" trap that I've exposed this
Metadata record instead of exposing a single function for each of its fields,
as I did originally. This record is not a bad way to do it, by any means,
but if this seems at odds with <https://youtu.be/x1FU3e0sT1I> - now you know why!
)

-}
type alias Internals =
    { body : String
    , created_at : Time.Posix
    , id : Int
    , slug : Slug
    , tags : List String
    , title : String
    }


type Preview
    = Preview


type Full
    = Full Body



-- INFO


post : Post a -> Internals
post (Post internals _) =
    internals


author : Post a -> Author
author (Post internals _) =
    -- uhhhhh @todo this sucks
    Author.IsAnonymous


slug : Post a -> Slug
slug (Post internals _) =
    internals.slug


body : Post Full -> Body
body (Post _ (Full extraInfo)) =
    extraInfo



-- TRANSFORM


{-| This is the only way you can transform an existing article:
you can change its author (e.g. to follow or unfollow them).
All other article data necessarily comes from the server!

We can tell this for sure by looking at the types of the exposed functions
in this module.

-}



-- mapAuthor : (Author -> Author) -> Post a -> Post a
-- mapAuthor transform (Post info extras) =
--     Post { info | author = transform info.author } extras


fromPreview : Body -> Post Preview -> Post Full
fromPreview newBody (Post info Preview) =
    Post info (Full newBody)



-- SERIALIZATION


previewDecoder : Maybe Cred -> Decoder (Post Preview)
previewDecoder maybeCred =
    Decode.succeed Post
        |> custom (internalsDecoder maybeCred)
        |> hardcoded Preview


fullDecoder : Maybe Cred -> Decoder (Post Full)
fullDecoder maybeCred =
    Decode.succeed Post
        |> custom (internalsDecoder maybeCred)
        |> required "body" (Decode.map Full Body.decoder)


intToPosix : Int -> Time.Posix
intToPosix ms =
    Time.millisToPosix (ms * 1000)


internalsDecoder : Maybe Cred -> Decoder Internals
internalsDecoder maybeCred =
    Decode.succeed Internals
        |> required "body" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "created_at"
            (Decode.map
                intToPosix
                Decode.int
            )
        |> required "id" Decode.int
        |> required "slug" Slug.decoder
        |> optional "tags" (Decode.list Decode.string) []
        |> required "title" Decode.string



-- |> required "favorited" Decode.bool
-- |> required "favoritesCount" Decode.int
-- SINGLE


fetch : Maybe Cred -> Slug -> Http.Request (Post Full)
fetch maybeCred articleSlug =
    Decode.field "article" (fullDecoder maybeCred)
        |> Api.get (Endpoint.article articleSlug) maybeCred



-- FAVORITE


favorite : Slug -> Cred -> Http.Request (Post Preview)
favorite articleSlug cred =
    Api.post (Endpoint.favorite articleSlug) (Just cred) Http.emptyBody (faveDecoder cred)


unfavorite : Slug -> Cred -> Http.Request (Post Preview)
unfavorite articleSlug cred =
    Api.delete (Endpoint.favorite articleSlug) cred Http.emptyBody (faveDecoder cred)


faveDecoder : Cred -> Decoder (Post Preview)
faveDecoder cred =
    Decode.field "article" (previewDecoder (Just cred))


{-| This is a "build your own element" API.

You pass it some configuration, followed by a `List (Attribute msg)` and a
`List (Html msg)`, just like any standard Html element.

-}
favoriteButton :
    Cred
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
favoriteButton _ msg attrs kids =
    toggleFavoriteButton "btn btn-sm btn-outline-primary" msg attrs kids


unfavoriteButton :
    Cred
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
unfavoriteButton _ msg attrs kids =
    toggleFavoriteButton "btn btn-sm btn-primary" msg attrs kids


toggleFavoriteButton :
    String
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
toggleFavoriteButton classStr msg attrs kids =
    Html.button
        (class classStr :: onClickStopPropagation msg :: attrs)
        (i [ class "ion-heart" ] [] :: kids)


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))
