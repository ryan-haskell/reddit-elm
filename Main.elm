module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style, placeholder, type', value)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (object1, tuple1, object4, list, int, bool, string, (:=))
import Task


-- MAIN


main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { subreddit : String
    , query : String
    , posts : List RedditPost
    , status : Maybe String
    }


initialSubreddit : String
initialSubreddit =
    "elm"


init : ( Model, Cmd Msg )
init =
    ( Model
        initialSubreddit
        ""
        []
        (Just "Loading...")
    , getRedditPosts initialSubreddit
    )



-- UPDATE


type Msg
    = UpdateQuery String
    | SearchClicked
    | RedditSuccess String
    | RedditFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery newQuery ->
            ( { model
                | query = newQuery
              }
            , Cmd.none
            )

        SearchClicked ->
            ( { model
                | subreddit = model.query
                , query = ""
                , status = Just "Loading..."
              }
            , getRedditPosts model.query
            )

        RedditSuccess response ->
            ( { model
                | posts = convertToPostList response
                , status = Nothing
              }
            , Cmd.none
            )

        RedditFail error ->
            ( { model
                | posts = []
                , status = Just (toString error)
              }
            , Cmd.none
            )



-- VIEW


type StyleClass
    = Container
    | SearchInput
    | SearchButton
    | CardList
    | Card
    | CardTitle
    | CardAuthor
    | StatusMessage


getStyle : StyleClass -> List ( String, String )
getStyle class =
    case class of
        Container ->
            [ ( "padding", "1rem 2rem" )
            ]

        SearchButton ->
            [ ( "width", "100px" )
            ]

        SearchInput ->
            [ ( "width", "calc(100% - 120px)" ) ]

        CardList ->
            []

        Card ->
            [ ( "padding", "10px" )
            , ( "border", "solid 1px #333" )
            , ( "marginBottom", "10px" )
            ]

        CardTitle ->
            [ ( "fontWeight", "bold" ) ]

        CardAuthor ->
            []

        StatusMessage ->
            [ ( "color", "#333" )
            , ( "textAlign", "center" )
            ]


viewPost : RedditPost -> Html Msg
viewPost post =
    div [ style (getStyle Card) ]
        [ h4 [ style (getStyle CardTitle) ] [ text post.title ]
        , p [ style (getStyle CardAuthor) ] [ text post.author ]
        ]


viewPosts : List RedditPost -> Html Msg
viewPosts posts =
    div [ style (getStyle CardList) ]
        (List.map viewPost posts)


view : Model -> Html Msg
view model =
    div [ style (getStyle Container) ]
        [ h1 [] [ text ("reddit.com/r/" ++ model.subreddit) ]
        , label [] [ text "Subreddit" ]
        , div [ onSubmit SearchClicked ]
            [ input
                [ placeholder model.subreddit
                , type' "text"
                , onInput UpdateQuery
                , style (getStyle SearchInput)
                , value model.query
                ]
                []
            , button
                [ style (getStyle SearchButton)
                , onClick SearchClicked
                ]
                [ text "Go" ]
            ]
        , p [] [ (viewPosts model.posts) ]
        , h3 [ style (getStyle StatusMessage) ]
            [ text (Maybe.withDefault "" model.status) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- FUNCTIONS


getRedditPosts : String -> Cmd Msg
getRedditPosts subreddit =
    Task.perform
        RedditFail
        RedditSuccess
        (Http.getString
            ("https://api.reddit.com/r/" ++ subreddit)
        )


type alias FirstLayer =
    { data : SecondLayer }


type alias SecondLayer =
    { children : List ThirdLayer }


type alias ThirdLayer =
    { data : RedditPost }


type alias RedditPost =
    { score : Int
    , isSelf : Bool
    , title : String
    , author : String
    }


convertToPostList : String -> List RedditPost
convertToPostList response =
    let
        redditStructure =
            Json.decodeString
                (object1
                    FirstLayer
                    ("data"
                        := (object1
                                SecondLayer
                                ("children"
                                    := (list
                                            (object1
                                                ThirdLayer
                                                ("data"
                                                    := (object4
                                                            RedditPost
                                                            ("score" := int)
                                                            ("is_self" := bool)
                                                            ("title" := string)
                                                            ("author" := string)
                                                       )
                                                )
                                            )
                                       )
                                )
                           )
                    )
                )
                response

        layerOne =
            Result.withDefault
                (FirstLayer (SecondLayer []))
                redditStructure

        posts =
            List.map
                (\obj -> RedditPost obj.data.score obj.data.isSelf obj.data.title obj.data.author)
                layerOne.data.children
    in
        posts
