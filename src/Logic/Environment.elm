module Logic.Environment exposing (Environment, Message, empty, init, style, subscriptions, update)

import Browser.Dom as Browser
import Browser.Events as Events
import Json.Decode as Decode
import Task
import VirtualDom


type alias Environment =
    { height : Int
    , width : Int
    , devicePixelRatio : Float
    , widthRatio : Float
    }


empty : Environment
empty =
    { height = 0
    , width = 0
    , devicePixelRatio = 0
    , widthRatio = 0
    }


type Message
    = Resize Int Int


subscriptions : Sub Message
subscriptions =
    Events.onResize Resize


update : Message -> Environment -> Environment
update msg model =
    case msg of
        Resize w h ->
            { model
                | width = w
                , height = h
                , widthRatio = toFloat w / toFloat h
            }


init : Decode.Value -> ( Environment, Cmd Message )
init flags =
    ( { height = 100
      , width = 100
      , widthRatio = 1
      , devicePixelRatio =
            flags
                |> Decode.decodeValue (Decode.field "devicePixelRatio" Decode.float)
                |> Result.withDefault 1
      }
    , requestWindowSize
    )


requestWindowSize : Cmd Message
requestWindowSize =
    Task.perform (\{ scene } -> Resize (round scene.width) (round scene.height)) Browser.getViewport


style : Environment -> List (VirtualDom.Attribute msg)
style env =
    [ toFloat env.width * env.devicePixelRatio |> round |> String.fromInt |> VirtualDom.attribute "width"
    , toFloat env.height * env.devicePixelRatio |> round |> String.fromInt |> VirtualDom.attribute "height"
    , VirtualDom.style "display" "block"
    , VirtualDom.style "width" (String.fromInt env.width ++ "px")
    , VirtualDom.style "height" (String.fromInt env.height ++ "px")
    ]
