import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Regex

main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias ValidationError =
  (String, String)

-- MODEL
type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , validationError: ValidationError
  }

model : Model
model =
  Model "" "" "" "" ("green", "")


-- UPDATE
type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Validate

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    Password password ->
      { model | password = password }
    PasswordAgain password ->
      { model | passwordAgain = password }
    Age age ->
      { model | age = age }
    Validate ->
      { model | validationError = validate model  }


password_pattern =
  Regex.regex("(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9]).{8,}")

validate : Model -> ValidationError
validate model =
  if model.password /= model.passwordAgain then
    ("red", "Passwords do not match!")
  else if not (Regex.contains password_pattern model.password) then
    ("red", "Passwords should be at least 8 chars long, and contains lover and Upper chars!")
  else if Result.withDefault -1 (String.toInt model.age) == -1 then
    ("red", "Age should be a number")
  else
    ("green", "OK")


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , br [] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , br [] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , br [] []
    , input [ type_ "text", placeholder "Age", onInput Age ] []
    , br [] []
    , input [ type_ "submit", onClick Validate
    ] []
    , viewValidation model
    ]

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      model.validationError
  in
    div [ style [("color", color)] ] [ text message ]

