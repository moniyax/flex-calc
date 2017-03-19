module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )


-- APP

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none )
        }


-- MODEL


type alias Model =
    { keyLog : List Key
    , result : Maybe String
    , buffer : List Key
    }

type Op =
    Divide | Plus | Times | Minus | Equal | Percent | ToggleSign | Clear

type Key =
    Op Op | Num Float | DotKey | ZeroDot

init : (Model, Cmd Msg)
init =
    ({ keyLog = []
     , result = Nothing
     , buffer = []
     }
     , Cmd.none)


-- UPDATE


type Msg = LogKey Key

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LogKey key ->
        (updateOnLogKey key model, Cmd.none)

updateOnLogKey : Key -> Model -> Model
updateOnLogKey key model =
    let model1 = { model | keyLog = key :: model.keyLog }
        buffer = runCalc model1
        result = Maybe.map bar <| firstNum buffer
    in { model1 | result = result, buffer = buffer}

firstNum : List Key -> Maybe Key
firstNum ks =
    List.foldl f Nothing ks

f : Key -> Maybe Key -> Maybe Key
f next acc =
    case (acc, next) of
        (Just num, _) -> Just num
        (Nothing, Num n) -> Just <| Num n
        (Nothing, ZeroDot) -> Just <| ZeroDot
        _            -> Nothing

bar : Key -> String
bar key =
    case key of
        (Num n) -> toString n
        ZeroDot -> "0."
        _       -> "0"

runCalc : Model -> List Key
runCalc model =
    List.foldr funk [] model.keyLog

funk : Key -> List Key -> List Key
funk key buffer =
    case (key, buffer) of
        (Op _, []) -> []
        (Num x, []) -> [Num x]
        (DotKey, []) -> [ZeroDot]
        (Num x, Num y :: tl) ->
            concatNums (Num x) (Num y) tl
        (Num x, ZeroDot :: tl) ->
            concatNums (Num x) ZeroDot tl
        (Op op, [Num x]) ->
            evalUnary [Op op, Num x] -- check for equal etc. ca_, [Op op, Num x]) -> [Op op_, Num x]
        (Num y, [Op op, Num x]) ->
            case op of
                Equal -> [Num y]
                _     -> [Num y, Op op, Num x]
        (Op op, [Num y, Op op_, Num x]) ->
             eval op [Num y, Op op_, Num x]
        _ -> buffer

concatNums : Key -> Key -> List Key -> List Key
concatNums num1 num2 tl =
    case (num1, num2) of
        (Num x, Num y) ->
            let newNum =
                (Num
                <| toFloat
                <| Result.withDefault 0
                <| String.toInt
                <| toString y ++ toString x)
            in newNum :: tl
        (Num x, ZeroDot) ->
            [Num (0.1 * x)]
        _ -> [Num 0] -- shouldn't get here. We dealing with nums here!

evalUnary : List Key -> List Key
evalUnary keys =
    case keys of
        [Op Percent, Num x] -> [Num <| x / 100]
        [Op ToggleSign, Num x] -> [Num <| -1 * x]
        [Op Equal, Num x] -> [Num x]
        [Op Clear, Num x] -> []
        _                 -> keys

eval : Op -> List Key -> List Key
eval op keys =
    let res = calc keys
    in
    case op of
        Plus    -> [Op op, res]
        Minus   -> [Op op, res]
        Divide  -> [Op op, res]
        Times   -> [Op op, res]
        Equal   -> [Op op, res]
        Percent -> [percentOfNum res]
        ToggleSign -> [toggleSignOfNum res]
        Clear   -> case List.tail keys of
                    Just v -> v
                    Nothing -> [] -- shouldn't get here cos in funk an op before an empty list is not possible

percentOfNum : Key -> Key
percentOfNum k =
            case k of
                Num n -> Num <| n / 100
                _ -> k -- Better to have a Num type so I don't have to do this.

toggleSignOfNum : Key -> Key
toggleSignOfNum k =
            case k of
                Num n -> Num <| -1 * n
                _ -> k -- Better to have a Num type so I don't have to do this.

calc : List Key -> Key
calc keys =
    case keys of
        [Num y, Op Plus, Num x] -> Num <| x + y
        [Num y, Op Minus, Num x] -> Num <| x - y
        [Num y, Op Divide, Num x] -> Num <| x / y
        [Num y, Op Times, Num x] -> Num <| x * y
        _ -> Num 1 -- shouldn't get here cos unary ops can't be sandmitched betwenn two numbers


-- VIEW


calcButton : String -> Key -> Html Msg
calcButton value key =
    let widthClass =
        case key of
            Num 0 -> "w-50"
            _     -> "w-25"
    in
    button [ class <| "btn btn-success h-25 key rounded-0 " ++ widthClass
           , onClick <| LogKey <| key ]
           [ text value ]


view : Model -> Html Msg
view model =
    let output = Maybe.withDefault "0"
                 <| model.result
    in
    div [class "calc-body d-flex flex-column h-100 w-25"]
        [
            div [class "calc-screen align-items-center bg-info bg-inverse d-flex h-25 justify-content-end p-1 pt-4 w-100"]
                [ text output ]
        ,   div [class "calc-keyboard align-content-start d-flex flex-wrap h-75 w-100"]
                [
                    calcButton "C" <| Op Clear
                ,   calcButton "+/-" <| Op ToggleSign
                ,   calcButton "%" <| Op Percent
                ,   calcButton "÷" <| Op Divide
                ,   calcButton "7" <| Num 7
                ,   calcButton "8" <| Num 8
                ,   calcButton "9" <| Num 9
                ,   calcButton "×" <| Op Times
                ,   calcButton "4" <| Num 4
                ,   calcButton "5" <| Num 5
                ,   calcButton "6" <| Num 6
                ,   calcButton "-" <| Op Minus
                ,   calcButton "1" <| Num 1
                ,   calcButton "2" <| Num 2
                ,   calcButton "3" <| Num 3
                ,   calcButton "+" <| Op Plus
                ,   calcButton "0" <| Num 0
                ,   calcButton "." <| DotKey
                ,   calcButton "=" <| Op Equal
                ]
        ]
