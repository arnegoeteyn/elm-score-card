port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (column, columnModifiers, columns, columnsModifiers)
import Bulma.Elements exposing (TitleSize(..), box, button, buttonModifiers, notification, notificationWithDelete, title)
import Bulma.Form
    exposing
        ( controlButton
        , controlInput
        , controlInputModifiers
        , controlLabel
        , controlModifiers
        , controlText
        , field
        , help
        , label
        )
import Bulma.Layout exposing (container)
import Bulma.Modifiers exposing (Color(..), HorizontalAlignment(..))
import Html exposing (Html, div, input, option, p, text)
import Html.Attributes exposing (class, classList, placeholder, src, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (int, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Maybe exposing (andThen)
import Time exposing (every)


port signIn : () -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


port signOut : () -> Cmd msg


port receiveRoutes : (Json.Encode.Value -> msg) -> Sub msg


port receiveRouteUpdate : (Json.Encode.Value -> msg) -> Sub msg


port updateRouteLog : Json.Encode.Value -> Cmd msg


port updateRoute : Json.Encode.Value -> Cmd msg


port receiveNotification : (Json.Encode.Value -> msg) -> Sub msg



---- MODEL ----


type AscentStyle
    = None
    | RedPoint
    | Flash


nextAscentStyle : AscentStyle -> AscentStyle
nextAscentStyle style =
    case style of
        RedPoint ->
            Flash

        Flash ->
            None

        None ->
            RedPoint


type alias ErrorData =
    { code : Maybe String, message : Maybe String, credential : Maybe String }


type alias UserData =
    { token : String, email : String, uid : String }


type alias Route =
    { id : String, name : String, grade : String, points : Int, number : Int, style : AscentStyle, logCount : Int }


type alias FormRoute =
    { id : String
    , name : String
    , grade : String
    , points : String
    , number : String
    }


type alias Notification =
    ( String, Int )


emptyNotification : Notification
emptyNotification =
    ( "", 0 )


defaultNotification : String -> Notification
defaultNotification =
    \x -> ( x, 5 )


emptyRoute : Route
emptyRoute =
    { id = "", name = "", grade = "", points = 0, number = 0, style = RedPoint, logCount = 0 }


emptyFormRoute : FormRoute
emptyFormRoute =
    { id = "", name = "", grade = "", points = "", number = "" }


type LeftPanel
    = ScoreCard
    | NewRoute


type alias Model =
    { userData : Maybe UserData
    , error : ErrorData
    , inputContent : String
    , routes : List Route
    , leftPanel : LeftPanel
    , formRoute : FormRoute
    , notification : Notification
    }


init : ( Model, Cmd Msg )
init =
    ( { userData = Maybe.Nothing
      , error = emptyError
      , inputContent = ""
      , routes = []
      , leftPanel = ScoreCard
      , formRoute = emptyFormRoute
      , notification = emptyNotification
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Decode.Error UserData)
    | LoggedInError (Result Json.Decode.Error ErrorData)
    | RoutesReceived (Result Json.Decode.Error (List Route))
    | RouteUpdateReceived (Result Json.Decode.Error Route)
    | NotificationReceived (Result Json.Decode.Error String)
    | RouteLogged Route
    | SetLeftPanel LeftPanel
    | UpdateForm FormUpdateMsg
    | Tick Time.Posix
    | ClearNotification


type FormUpdateMsg
    = UpdateName String
    | UpdateGrade String
    | UpdatePoints String
    | SaveForm


emptyError : ErrorData
emptyError =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Maybe.Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, signIn () )

        LogOut ->
            ( { model | userData = Maybe.Nothing, error = emptyError }, signOut () )

        Tick _ ->
            ( tickNotification model, Cmd.none )

        ClearNotification ->
            ( { model | notification = emptyNotification }, Cmd.none )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { model | userData = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { model | error = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        RouteUpdateReceived result ->
            case result of
                Ok route ->
                    ( { model | routes = replaceRoute model.routes route }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        RoutesReceived result ->
            case result of
                Ok value ->
                    ( { model | routes = List.sortBy .number value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        NotificationReceived result ->
            case result of
                Ok value ->
                    ( { model | notification = defaultNotification value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        RouteLogged route ->
            let
                ( updatedModel, updatedRoute ) =
                    nextLogStatus model route
            in
            ( updatedModel, updateRouteLog <| loggedRouteEncoder model updatedRoute )

        SetLeftPanel leftPanel ->
            ( { model | leftPanel = leftPanel }, Cmd.none )

        UpdateForm updateMsg ->
            let
                ( updatedRoute, updatedMsg ) =
                    formUpdate updateMsg model
            in
            ( { model | formRoute = updatedRoute }, updatedMsg )


formUpdate : FormUpdateMsg -> Model -> ( FormRoute, Cmd msg )
formUpdate msg model =
    let
        route =
            model.formRoute
    in
    case msg of
        UpdateGrade grade ->
            ( { route | grade = grade }, Cmd.none )

        UpdateName name ->
            ( { route | name = name }, Cmd.none )

        UpdatePoints points ->
            ( { route | points = points }, Cmd.none )

        SaveForm ->
            ( emptyFormRoute, updateRoute <| routeEncoder model route )


tickNotification : Model -> Model
tickNotification model =
    case model.notification of
        ( _, 0 ) ->
            model

        ( v, x ) ->
            { model | notification = ( v, x - 1 ) }


nextLogStatus : Model -> Route -> ( Model, Route )
nextLogStatus model route =
    let
        nextStyle =
            nextAscentStyle route.style
    in
    let
        updatedRoute =
            { route
                | style = nextStyle
                , logCount =
                    case nextStyle of
                        None ->
                            route.logCount - 1

                        RedPoint ->
                            route.logCount + 1

                        Flash ->
                            route.logCount
            }
    in
    ( { model | routes = replaceRoute model.routes updatedRoute }, updatedRoute )


replaceRoute : List Route -> Route -> List Route
replaceRoute routes route =
    case routes of
        [] ->
            []

        first :: latter ->
            (if first.id == route.id then
                route

             else
                first
            )
                :: replaceRoute latter route


loggedRouteEncoder : Model -> Route -> Json.Encode.Value
loggedRouteEncoder model route =
    Json.Encode.object
        [ ( "routeId", Json.Encode.string route.id )
        , ( "style", Json.Encode.string <| styleEncoder route.style )
        , ( "uid"
          , case model.userData of
                Just userData ->
                    Json.Encode.string userData.uid

                Maybe.Nothing ->
                    Json.Encode.null
          )
        ]


routeEncoder : Model -> FormRoute -> Json.Encode.Value
routeEncoder model route =
    Json.Encode.object
        [ ( "routeId", Json.Encode.string route.id )
        , ( "route"
          , Json.Encode.object
                [ ( "grade", Json.Encode.string route.grade )
                , ( "points"
                  , case String.toInt route.points of
                        Just p ->
                            Json.Encode.int p

                        Nothing ->
                            Json.Encode.int 0
                  )
                , ( "number", Json.Encode.int <| 1 + List.length model.routes )
                , ( "name", Json.Encode.string route.name )
                ]
          )
        ]


messageToError : String -> ErrorData
messageToError message =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Just message }


errorPrinter : ErrorData -> String
errorPrinter errorData =
    Maybe.withDefault "" errorData.code ++ " " ++ Maybe.withDefault "" errorData.credential ++ " " ++ Maybe.withDefault "" errorData.message



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , case model.userData of
            Just _ ->
                logOutButton

            Maybe.Nothing ->
                logInButton
        , container [ class "is-fluid" ]
            [ columns columnsModifiers
                []
                [ renderLeftPanel model
                , column columnModifiers [] [ title H2 [] [ text "Ranking" ] ]
                ]
            , title H2 [] [ text <| errorPrinter model.error ]
            ]
        ]


logOutButton : Html Msg
logOutButton =
    button buttonModifiers [ onClick LogOut ] [ text "Logout" ]


logInButton : Html Msg
logInButton =
    button buttonModifiers [ onClick LogIn ] [ text "Login" ]


leftPanelSwitchButton : LeftPanel -> Html Msg
leftPanelSwitchButton switchTo =
    button buttonModifiers [ onClick <| SetLeftPanel switchTo ] [ text "x" ]


renderLeftPanel : Model -> Html Msg
renderLeftPanel model =
    column columnModifiers
        []
    <|
        case model.leftPanel of
            ScoreCard ->
                [ title H2 [] [ text "Score card", leftPanelSwitchButton NewRoute ]
                , renderRoutes model.routes
                ]

            NewRoute ->
                [ title H2 [] [ text "New route", leftPanelSwitchButton ScoreCard ]
                , renderRouteForm emptyFormRoute
                , renderNotification model.notification
                ]


renderRouteForm : FormRoute -> Html Msg
renderRouteForm route =
    div []
        [ field []
            [ controlLabel [] [ text "Name" ]
            , controlText controlInputModifiers [] [ value route.name, onInput (UpdateForm << UpdateName) ] []
            ]
        , field []
            [ controlLabel [] [ text "Grade" ]
            , controlText controlInputModifiers [] [ value route.grade, onInput (UpdateForm << UpdateGrade) ] []
            ]
        , field []
            [ controlLabel [] [ text "Points" ]
            , controlInput controlInputModifiers [] [ value route.points, onInput (UpdateForm << UpdatePoints) ] []
            ]
        , field [] [ controlButton buttonModifiers [] [ onClick (UpdateForm SaveForm) ] [ text "Save" ] ]
        ]


renderNotification : Notification -> Html Msg
renderNotification ( value, remainingTime ) =
    if remainingTime == 0 then
        text ""

    else
        notificationWithDelete Info [] ClearNotification [ text value ]


renderRoutes : List Route -> Html Msg
renderRoutes routes =
    columns columnsModifiers [ classList [ ( "is-mobile", True ), ( "is-multiline", True ), ( "is-variable", True ) ] ] <|
        List.map renderRoute routes


renderRoute : Route -> Html Msg
renderRoute route =
    column columnModifiers
        [ class "is-one-fifth" ]
        [ box
            [ onClick <| RouteLogged route, class <| styleEncoder route.style, class "route-box" ]
            [ text <| String.fromInt route.number, text " - ", text <| String.fromInt route.logCount ]
        ]



---- DECODER/ENCODERS ----


userDataDecoder : Json.Decode.Decoder UserData
userDataDecoder =
    Json.Decode.succeed UserData
        |> Json.Decode.Pipeline.required "token" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string


logInErrorDecoder : Json.Decode.Decoder ErrorData
logInErrorDecoder =
    Json.Decode.succeed ErrorData
        |> Json.Decode.Pipeline.required "code" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "message" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "credential" (Json.Decode.nullable Json.Decode.string)


routeListDecoder : Json.Decode.Decoder (List Route)
routeListDecoder =
    Json.Decode.succeed identity
        |> Json.Decode.Pipeline.required "routes" (Json.Decode.list routeDecoder)


routeIdDecoder : Json.Decode.Decoder String
routeIdDecoder =
    Json.Decode.field "routeId" Json.Decode.string


routeUpdateDecoder : Model -> Json.Decode.Decoder Route
routeUpdateDecoder model =
    routeIdDecoder
        |> Json.Decode.andThen
            (\routeId ->
                let
                    routes =
                        List.filter (\route -> routeId == route.id) model.routes
                in
                case routes of
                    [] ->
                        Json.Decode.fail "test"

                    first :: _ ->
                        updateRouteDecoder first
            )


routeDecoder : Json.Decode.Decoder Route
routeDecoder =
    updateRouteDecoder emptyRoute


updateRouteDecoder : Route -> Json.Decode.Decoder Route
updateRouteDecoder route =
    Json.Decode.succeed Route
        |> optional "id" string route.id
        |> optional "name" string route.name
        |> optional "grade" string route.grade
        |> optional "points" int route.points
        |> optional "number" int route.number
        |> optional "style" styleDecoder route.style
        |> optional "logCount" int route.logCount


styleDecoder : Json.Decode.Decoder AscentStyle
styleDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case String.toLower s of
                    "redpoint" ->
                        Json.Decode.succeed RedPoint

                    "flash" ->
                        Json.Decode.succeed Flash

                    "none" ->
                        Json.Decode.succeed None

                    _ ->
                        Json.Decode.fail <| "unknown style: " ++ s
            )


styleEncoder : AscentStyle -> String
styleEncoder style =
    case style of
        RedPoint ->
            "redpoint"

        None ->
            "none"

        Flash ->
            "flash"



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ signInInfo (Json.Decode.decodeValue userDataDecoder >> LoggedInData)
        , signInError (Json.Decode.decodeValue logInErrorDecoder >> LoggedInError)
        , receiveRoutes (Json.Decode.decodeValue routeListDecoder >> RoutesReceived)
        , receiveRouteUpdate (Json.Decode.decodeValue (routeUpdateDecoder model) >> RouteUpdateReceived)
        , receiveNotification (Json.Decode.decodeValue Json.Decode.string >> NotificationReceived)
        , Time.every 1000 Tick
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
