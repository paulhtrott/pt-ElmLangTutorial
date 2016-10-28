import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Regex


main =
    App.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : Maybe Int
    }

model : Model
model =
    Model "" "" "" Nothing

-- UPDATE
type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }
        Age age ->
            { model | age = convertStringToInt age }
        Password password ->
            { model | password = password }
        PasswordAgain password ->
            { model | passwordAgain = password }

-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ input [ type' "text", placeholder "Name", onInput Name] []
        , input [ type' "text", placeholder "Enter Age", onInput Age] []
        , input [ type' "text", placeholder "Password", onInput Password] []
        , input [ type' "text", placeholder "Re-enter Password", onInput PasswordAgain] []
        , button [] [ text "Validate Form" ]
        , viewValidation model
        ]

viewValidation : Model -> Html msg
viewValidation model =
    let
        (color, message) =
            if  model.age == Nothing then
                ("red", "Age must be a number greater than zero")
            else if String.length model.password <= 8 then
                ("red", "Password must be greater than 8 characters")
            else if not(Regex.contains (Regex.regex "(?=.*[a-z])(?=.*[A-Z])(?=.*\\d).+") model.password) then
                ("red", "Password must contain upper case, lower case and numeric characters")
            else if model.password == model.passwordAgain then
                ("green", "OK")
            else
                ("red", "Passwords do not match!")
    in
        div [ style [("color", color)] ] [ text message ]


convertStringToInt : String -> Maybe Int
convertStringToInt stringToConvert =
    case String.toInt stringToConvert of
        Err _ -> Nothing
        Ok converted -> Just converted     
