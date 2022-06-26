module Post.Tag exposing (Tag, list, toString)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Http
import Json.Decode as Decode exposing (Decoder)



-- TYPES


type Tag
    = Tag String



-- TRANSFORM


toString : Tag -> String
toString (Tag slug) =
    slug



-- LIST


list : Http.Request (List Tag)
list =
    let
        decodeResponse =
            Decode.field "values" (Decode.list decoder)
    in
    Api.get Endpoint.tags Nothing decodeResponse



-- SERIALIZATION


decoder : Decoder Tag
decoder =
    Decode.map Tag (Decode.field "tag" Decode.string)
