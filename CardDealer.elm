module CardDealer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


-- MODEL


type alias Model =
    List ( String, String )


ranks =
    [ "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A" ]


suits =
    [ "♣️", "♦️", "♥️", "♠️" ]


initialModel : Model
initialModel =
    zip ranks suits -- [("A","♣️"),("A","♦️"),("A","♥️"),("A","♠️"),("2","♣️"),("2","♦️"), ...]


shuffle : Model -> List Int -> Model
shuffle deck randoms =
    List.map2 (,) randoms deck      -- [(32,("A","♣️")), (19,("A","♦️")), (1,("A","♥️")), ... ]
        |> List.sortBy Tuple.first  -- [(1,("K","♦️")), (2,("10","♥️")), (3,("8","♣️")), ... ]
        |> List.unzip               -- ([ 1,2,3 ], [ ("K","♦️"), ("10","♥️"), ("8","♣️") ... ])
        |> Tuple.second             -- [ ("K","♦️"), ("10","♥️"), ("8","♣️") ... ]


zip : List a -> List b -> List ( a, b )
zip xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs


-- UPDATE


type Msg
    = Deal
    | New52Randoms (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Deal ->
            ( model, generate52Randoms )

        New52Randoms randoms ->
            ( (shuffle model randoms), Cmd.none )



-- COMMANDS


generate52Randoms : Cmd Msg
generate52Randoms =
    Random.int 0 100
        |> Random.list 52
        |> Random.generate New52Randoms


-- VIEW


view : Model -> Html Msg
view model =
    let
        hand1 =
            List.take 26 model

        hand2 =
            List.drop 26 model
    in
        div []
            [ button [ onClick Deal ] [ text "Deal!" ]
            , h3 [] [ text "Player 1" ]
            , viewCards hand1
            , h3 [] [ text "Player 2" ]
            , viewCards hand2
            ]


viewCards : List ( String, String ) -> Html Msg
viewCards cards =
    div [] (List.map viewCard cards)


viewCard : ( String, String ) -> Html Msg
viewCard card =
    div [ cardStyle ]
        [ span [] [ text (Tuple.first card) ]
        , span [] [ text (Tuple.second card) ]
        ]


cardStyle : Attribute msg
cardStyle =
    style
        [ ( "display", "inline-block" )
        , ( "background", "#ccc" )
        , ( "border-radius", "4px" )
        , ( "margin", "10px" )
        , ( "padding", "10px" )
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, generate52Randoms )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
