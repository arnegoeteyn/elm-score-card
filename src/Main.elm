port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (column, columnModifiers, columns, columnsModifiers)
import Bulma.Components exposing (card, cardContent, cardFooter, cardFooterItem, cardFooterItemLink, cardHeader, cardTitle)
import Bulma.Elements exposing (TitleSize(..), box, button, buttonModifiers, notification, notificationWithDelete, title)
import Bulma.Form
    exposing
        ( controlButton
        , controlInput
        , controlInputModifiers
        , controlLabel
        , controlText
        , field
        )
import Bulma.Layout exposing (container)
import Bulma.Modifiers exposing (Color(..), HorizontalAlignment(..))
import Html exposing (Html, a, div, input, option, p, text)
import Html.Attributes exposing (class, classList, placeholder, src, style, value)
import Html.Events exposing (onClick, onInput, onMouseDown, onMouseUp)
import Json.Decode exposing (int, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Maybe exposing (andThen)
import Time


port signIn : () -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


port signOut : () -> Cmd msg


port receiveRoutes : (Json.Encode.Value -> msg) -> Sub msg


port receiveRouteUpdate : (Json.Encode.Value -> msg) -> Sub msg


port receiveRanking : (Json.Encode.Value -> msg) -> Sub msg


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


nextLockStatus : LockStatus -> LockStatus
nextLockStatus lockStatus =
    case lockStatus of
        Locked ->
            Editable

        Editable ->
            Locked


type alias ErrorData =
    { code : Maybe String, message : Maybe String, credential : Maybe String }


type UserPrivilege
    = User
    | Admin


type alias UserData =
    { token : String, email : String, uid : String, privilege : UserPrivilege }


type alias Route =
    { id : String, name : String, grade : String, points : Int, number : Int, style : AscentStyle, logCount : Int, lockStatus : LockStatus }


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
    { id = "", name = "", grade = "", points = 0, number = 0, style = RedPoint, logCount = 0, lockStatus = Editable }


emptyFormRoute : FormRoute
emptyFormRoute =
    { id = "", name = "", grade = "", points = "", number = "" }


type LeftPanel
    = ScoreCard
    | NewRoute


type LockStatus
    = Editable
    | Locked


type alias Ranking =
    { id : String
    , position : Int
    , points : Int
    , routesClimbed : Int
    , name : String
    }


type alias Model =
    { userData : Maybe UserData
    , error : ErrorData
    , inputContent : String
    , routes : List Route
    , leftPanel : LeftPanel
    , formRoute : FormRoute
    , notification : Notification
    , routeClickTime : Maybe Int
    , clickedRoute : Maybe Route
    , ranking : List Ranking
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
      , routeClickTime = Nothing
      , clickedRoute = Nothing
      , ranking = []
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
    | RankingReceived (Result Json.Decode.Error (List Ranking))
    | NotificationReceived (Result Json.Decode.Error String)
    | SetLeftPanel LeftPanel
    | UpdateForm FormUpdateMsg
    | Tick Time.Posix
    | ClearNotification
    | ModifyRouteMsg ModifyRouteMsg
    | RouteLocked Route


type ModifyRouteMsg
    = RouteLogged Route


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
            let
                newModel =
                    (tickNotification >> tickClickTime) model
            in
            case newModel.clickedRoute of
                Nothing ->
                    ( newModel, Cmd.none )

                Just route ->
                    if Maybe.withDefault 0 newModel.routeClickTime >= 1 then
                        update (ModifyRouteMsg (RouteLogged route)) newModel

                    else
                        ( newModel, Cmd.none )

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

        SetLeftPanel leftPanel ->
            ( { model | leftPanel = leftPanel }, Cmd.none )

        UpdateForm updateMsg ->
            let
                ( updatedRoute, updatedMsg ) =
                    formUpdate updateMsg model
            in
            ( { model | formRoute = updatedRoute }, updatedMsg )

        RankingReceived result ->
            case result of
                Ok ranking ->
                    ( { model | ranking = ranking }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        RouteLocked route ->
            let
                newRoute =
                    { route | lockStatus = nextLockStatus route.lockStatus }
            in
            ( { model | routes = replaceRoute model.routes newRoute }, updateRouteCmd model newRoute )

        ModifyRouteMsg modifyMsg ->
            modifyRouteUpdate modifyMsg model


ifNotLocked : (Route -> a) -> Route -> a -> a
ifNotLocked f route default =
    case route.lockStatus of
        Locked ->
            default

        Editable ->
            f route


updateRouteCmd : Model -> Route -> Cmd msg
updateRouteCmd model updatedRoute =
    updateRouteLog <| loggedRouteEncoder model updatedRoute


modifyRouteUpdate : ModifyRouteMsg -> Model -> ( Model, Cmd msg )
modifyRouteUpdate msg model =
    case msg of
        RouteLogged route ->
            let
                ( updatedModel, updatedRoute ) =
                    ifNotLocked (nextLogStatus model) route ( model, route )
            in
            ( updatedModel
            , ifNotLocked (\_ -> updateRouteCmd model updatedRoute) route Cmd.none
            )


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
            ( emptyFormRoute, updateRoute <| formRouteEncoder model route )


tickClickTime : Model -> Model
tickClickTime model =
    { model
        | routeClickTime =
            model.routeClickTime
                |> andThen (\x -> Just <| x + 1)
    }


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
                , column columnModifiers
                    []
                    [ renderPersonalInformation model.userData
                    , div [] [ title H2 [] [ text "Ranking" ] ]
                    , div [] <|
                        List.map
                            (\ranking ->
                                div []
                                    [ text (String.fromInt ranking.position ++ " " ++ ranking.name ++ " " ++ String.fromInt ranking.points)
                                    ]
                            )
                            model.ranking
                    ]
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
    button buttonModifiers
        [ onClick <| SetLeftPanel switchTo ]
        [ text <|
            case switchTo of
                ScoreCard ->
                    "x"

                NewRoute ->
                    "+"
        ]


renderPersonalInformation : Maybe UserData -> Html Msg
renderPersonalInformation userData =
    case userData of
        Nothing ->
            div [] []

        Just user ->
            div []
                [ p [] [ text <| privilegeEncoder user.privilege ]
                , p [] [ text user.email ]
                , p [] [ text user.uid ]
                ]


renderLeftPanel : Model -> Html Msg
renderLeftPanel model =
    column columnModifiers
        []
    <|
        case model.leftPanel of
            ScoreCard ->
                [ renderTitle "Scorecard" NewRoute
                , renderRoutes model.routes
                ]

            NewRoute ->
                [ renderTitle "New route" ScoreCard
                , renderRouteForm emptyFormRoute
                , renderNotification model.notification
                ]


renderTitle : String -> LeftPanel -> Html Msg
renderTitle titleString switchTo =
    columns columnsModifiers
        []
        [ column columnModifiers [ class "is-four-fifths" ] [ title H2 [] [ text titleString ] ]
        , column columnModifiers [ class "is-offset-1" ] [ leftPanelSwitchButton switchTo ]
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
        [ card [ classList [ ( "grayout", route.lockStatus == Locked ) ] ]
            [ cardHeader
                [ class <| styleEncoder route.style
                , class "route-box"
                ]
                [ cardTitle []
                    [ title H6 [] [ text <| "#" ++ String.fromInt route.number ++ " - " ++ styleEncoder route.style ]
                    ]
                ]
            , cardContent []
                [ p [] [ text <| "Logged: " ++ String.fromInt route.logCount ]
                , p []
                    [ text <|
                        (String.fromFloat <|
                            if route.logCount /= 0 then
                                toFloat route.points / toFloat route.logCount

                            else
                                toFloat route.points
                        )
                            ++ "/"
                            ++ String.fromInt route.points
                    ]
                ]
            , cardFooter []
                [ cardFooterItemLink [ onClick <| ModifyRouteMsg (RouteLogged route) ] [ text "log" ]
                , cardFooterItemLink [] [ text "edit" ]
                , cardFooterItemLink [ onClick <| RouteLocked route ] [ text "lock" ]
                ]
            ]
        ]



---- DECODER/ENCODERS ----


userDataDecoder : Json.Decode.Decoder UserData
userDataDecoder =
    Json.Decode.succeed UserData
        |> Json.Decode.Pipeline.required "token" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string
        |> Json.Decode.Pipeline.optional "privilege" privilegeDecoder User


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


rankingListDecoder : Json.Decode.Decoder (List Ranking)
rankingListDecoder =
    Json.Decode.succeed identity
        |> Json.Decode.Pipeline.required "ranking" (Json.Decode.list rankingDecoder)


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


rankingDecoder : Json.Decode.Decoder Ranking
rankingDecoder =
    Json.Decode.succeed Ranking
        |> required "id" string
        |> required "position" int
        |> required "points" int
        |> required "routesClimbed" int
        |> required "name" string


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
        |> optional "lockStatus" lockStatusDecoder route.lockStatus


lockStatusDecoder : Json.Decode.Decoder LockStatus
lockStatusDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case String.toLower s of
                    "locked" ->
                        Json.Decode.succeed Locked

                    _ ->
                        Json.Decode.succeed Editable
            )


privilegeDecoder : Json.Decode.Decoder UserPrivilege
privilegeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case String.toLower s of
                    "admin" ->
                        Json.Decode.succeed Admin

                    _ ->
                        Json.Decode.succeed User
            )


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


loggedRouteEncoder : Model -> Route -> Json.Encode.Value
loggedRouteEncoder model route =
    Json.Encode.object
        [ ( "routeId", Json.Encode.string route.id )
        , ( "style", Json.Encode.string <| styleEncoder route.style )
        , ( "lockStatus", Json.Encode.string <| lockStatusEncoder route.lockStatus )
        , ( "uid"
          , case model.userData of
                Just userData ->
                    Json.Encode.string userData.uid

                Maybe.Nothing ->
                    Json.Encode.null
          )
        ]


formRouteEncoder : Model -> FormRoute -> Json.Encode.Value
formRouteEncoder model route =
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


lockStatusEncoder : LockStatus -> String
lockStatusEncoder lockStatus =
    case lockStatus of
        Locked ->
            "locked"

        Editable ->
            "editable"


styleEncoder : AscentStyle -> String
styleEncoder style =
    case style of
        RedPoint ->
            "redpoint"

        None ->
            "none"

        Flash ->
            "flash"


privilegeEncoder : UserPrivilege -> String
privilegeEncoder privilege =
    case privilege of
        Admin ->
            "admin"

        User ->
            "user"



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ signInInfo (Json.Decode.decodeValue userDataDecoder >> LoggedInData)
        , signInError (Json.Decode.decodeValue logInErrorDecoder >> LoggedInError)
        , receiveRoutes (Json.Decode.decodeValue routeListDecoder >> RoutesReceived)
        , receiveRouteUpdate (Json.Decode.decodeValue (routeUpdateDecoder model) >> RouteUpdateReceived)
        , receiveRanking (Json.Decode.decodeValue rankingListDecoder >> RankingReceived)
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
