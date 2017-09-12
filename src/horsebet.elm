module Main exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, float, int, string)
import Json.Decode.Pipeline exposing (decode, required)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { pools : List Pool
    , selectedPool : String
    , suggestion : Suggestion
    , message : String
    }


type alias Generator =
    { generators : List Pool
    }


type alias Pool =
    { betType : String
    , date : List Int
    , trackCode : Int
    , raceNumber : Int
    , flavour : String
    }


type alias Suggestion =
    { legs : List Leg
    }


type alias Leg =
    { selections : List Int
    }


initalPools =
    let
        result =
            Json.Decode.decodeString decodeGenerators
                """
                {
                    "generators": [
                        {
                            "betType": "GS75",
                            "date": [
                                2017,
                                9,
                                11
                            ],
                            "trackCode": 6,
                            "raceNumber": 5,
                            "flavour": "FAVORIT_HARRY"
                        },
                         {
                           "betType": "V75",
                           "date": [
                             2017,
                             9,
                             11
                           ],
                           "trackCode": 5,
                           "raceNumber": 4,
                           "flavour": "FAVORIT_HARRY"
                         },
                         {
                           "betType": "V75",
                           "date": [
                             2017,
                             9,
                             11
                           ],
                           "trackCode": 5,
                           "raceNumber": 4,
                           "flavour": "CHANS_HARRY"
                         }
                    ]
                }
                """
    in
    case result of
        Err _ ->
            []

        Ok generator ->
            generator.generators


init : ( Model, Cmd Msg )
init =
    ( { pools = initalPools
      , suggestion = { legs = [] }
      , message = "Please select something"
      , selectedPool = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewSuggestion (Result Http.Error Suggestion)
    | GenerateSuggestion
    | SetPool String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NewSuggestion (Ok suggestion) ->
            ( { model | suggestion = suggestion }, Cmd.none )

        NewSuggestion (Err _) ->
            ( { model | message = "Error!" }, Cmd.none )

        GenerateSuggestion ->
            ( model, generateSuggestion model )

        SetPool poolName ->
            ( { model | selectedPool = poolName }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Pools" ]
        , h2 [] [ text "Make suggestion" ]
        , poolsView model
        , button [ onClick GenerateSuggestion ] [ text "Get suggestion" ]
        , hr [] []
        , p []
            [ text model.message
            ]
        , div []
            [ suggestionTable model
            ]
        ]


poolsView : Model -> Html Msg
poolsView model =
    select [ onInput SetPool ]
        (List.map poolView model.pools)


poolView : Pool -> Html Msg
poolView pool =
    option []
        [ text (poolToString pool)
        ]


poolToString : Pool -> String
poolToString pool =
    pool.betType
        ++ " "
        ++ toString pool.date
        ++ " "
        ++ toString pool.trackCode
        ++ " "
        ++ toString pool.raceNumber
        ++ " "
        ++ pool.flavour


suggestionTable : Model -> Html Msg
suggestionTable model =
    table []
        [ tableHead
        , tableBody model
        ]


tableHead : Html Msg
tableHead =
    thead []
        [ tr []
            [ th [] [ text "Leg" ]
            , th [] [ text "Suggestion" ]
            ]
        ]


tableBody : Model -> Html Msg
tableBody model =
    tbody []
        (List.map2 legRow (List.range 1 8) (List.map mapLegToHtml model.suggestion.legs))


legRow : Int -> Html Msg -> Html Msg
legRow legNo suggestion =
    tr []
        [ td [] [ text (toString legNo) ]
        , td [] [ suggestion ]
        ]


mapLegToHtml : Leg -> Html Msg
mapLegToHtml leg =
    text (String.join ", " (List.map toString leg.selections))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


decodeGenerators : Decoder Generator
decodeGenerators =
    decode Generator
        |> required "generators" (Json.Decode.list decodePool)


decodePool : Decoder Pool
decodePool =
    decode Pool
        |> required "betType" string
        |> required "date" (Json.Decode.list int)
        |> required "trackCode" int
        |> required "raceNumber" int
        |> required "flavour" string


generateSuggestion : Model -> Cmd Msg
generateSuggestion model =
    let
        url =
            "http://localhost:8080/suggestions"

        request =
            Http.post url Http.emptyBody decodeSuggestion
    in
    Http.send NewSuggestion request


decodeSuggestion : Decoder Suggestion
decodeSuggestion =
    decode Suggestion
        |> required "legs" (Json.Decode.list decodeLeg)


decodeLeg : Decoder Leg
decodeLeg =
    decode Leg
        |> required "selections" (Json.Decode.list int)
