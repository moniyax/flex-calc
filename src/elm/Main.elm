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
    , buffer : Buffer
    }

type Op =
    Divide | Plus | Times | Minus | Equal | Percent | ToggleSign | Clear

type Key =
    OpKey Op | NumKey Float | DotKey

type Arith =
    Op Op | Num Float | NumDot Float | ZeroDot

type alias Buffer = List Arith
type alias KeyLog = List Key

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

firstNum : Buffer -> Maybe Arith
firstNum vals =
    List.foldl f Nothing vals

f : Arith -> Maybe Arith -> Maybe Arith
f next acc =
    case (acc, next) of
        (Just num, _) -> Just num
        (Nothing, Num n) -> Just <| Num n
        (Nothing, ZeroDot) -> Just <| ZeroDot
        (Nothing, NumDot n) -> Just <| NumDot n
        _            -> Nothing

bar : Arith -> String
bar val =
    case val of
        Num n       -> toString n
        ZeroDot     -> "0."
        NumDot n    -> toString n ++ "."
        _           -> "0"

runCalc : Model -> Buffer
runCalc model =
    List.foldr funk [] model.keyLog

updateFirstNumInBuffer : (Arith -> Arith) -> Buffer -> Buffer
updateFirstNumInBuffer f buffer =
    let foo val acc =
            case (val, acc) of
                (Num x, (vals, Nothing)) ->
                    ((f <| Num x) :: vals, Just <| Num x)
                (val, (vals, maybe)) ->
                    (val :: vals, maybe)
    in case List.foldr foo ([], Nothing) buffer of
            (newBuffer, _) -> newBuffer

funk : Key -> Buffer -> Buffer
funk key buffer =
    case (key, buffer) of
        -- (DotKey  , buffer)  ->
        --     let addDotToNum num =
        --         case num of
        --             (Num x) ->  if String.contains "." (toString x) then
        --                             Num x
        --                         else
        --                             Num
        --                             <| Result.withDefault 0
        --                             <| String.toFloat
        --                             <| toString x ++ "."
        --             _       ->  Num 9999 -- Shouldn't get here. We onl matching Nums.
        --     in  updateFirstNumInBuffer addDotToNum buffer
        (DotKey  , [])      -> [ZeroDot]
        (DotKey, Num x :: tl) -> NumDot x :: tl
        (DotKey, NumDot x :: tl) -> NumDot x :: tl
        (DotKey, [Op op, Num x]) ->
            case op of
                Equal -> [ZeroDot]
                _     -> [ZeroDot, Op op, Num x]
        --
        (DotKey, Op op :: tl) -> ZeroDot :: Op op :: tl
        (DotKey, ZeroDot :: tl) -> ZeroDot :: tl

        (NumKey x, [])              -> [Num x]
        (NumKey x, NumDot y :: tl)  ->
            (Num <| Result.withDefault 0
                 <| String.toFloat
                 <| toString y ++ "." ++ toString x) :: tl
        (NumKey x, Num y :: tl)  ->
            (concatNums (Num x) (Num y)) :: tl
        (NumKey x, ZeroDot :: tl)  ->
            (concatNums (Num x) ZeroDot) :: tl
        (NumKey y, [Op op, Num x]) ->
            case op of
                Equal -> [Num y]
                _     -> [Num y, Op op, Num x]

        (OpKey _ , [])      -> []
        (OpKey op, Op Equal :: tl) ->
            Op op :: tl
        (OpKey op, [ZeroDot]) ->
            evalUnary [Op op, Num 0]
        (OpKey op, [ZeroDot, Op op_, Num x]) ->
             eval op [Num 0, Op op_, Num x]
        (OpKey op, [NumDot x]) ->
            evalUnary [Op op, Num x]
        (OpKey op, [NumDot x, Op op_, Num y]) ->
             eval op [Num x, Op op_, Num y]
        (OpKey op, [Num x]) ->
            evalUnary [Op op, Num x] -- check for equal etc.

        (OpKey op, [Num y, Op op_, Num x]) ->
             eval op [Num y, Op op_, Num x]
        _ -> buffer

concatNums : Arith -> Arith -> Arith
concatNums num1 num2 =
    case (num1, num2) of
        (Num x, Num y) ->
            Num
            <| Result.withDefault 0
            <| String.toFloat
            <| toString y ++ toString x
        (Num x, ZeroDot) ->
            Num
            <| Result.withDefault 0
            <| String.toFloat
            <| "0." ++ toString x
        _ -> Num 0 -- shouldn't get here. We dealing with nums here!

evalUnary : Buffer -> Buffer
evalUnary vals =
    case vals of
        [Op Percent, Num x]     -> [Num <| x / 100]
        [Op ToggleSign, Num x]  -> [Num <| -1 * x]
        [Op Equal, Num x] -> [Op Equal, Num x]
        [Op Clear, Num x] -> []
        _                 -> vals

eval : Op -> Buffer -> Buffer
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

calc : Buffer -> Arith
calc keys =
    case keys of
        [Num y, Op Plus, Num x]     -> Num <| x + y
        [Num y, Op Minus, Num x]    -> Num <| x - y
        [Num y, Op Divide, Num x]   -> Num <| x / y
        [Num y, Op Times, Num x]    -> Num <| x * y
        _ -> Num 1 -- shouldn't get here cos unary ops can't be sandmitched betwenn two numbers

percentOfNum : Arith -> Arith
percentOfNum k =
            case k of
                Num n -> Num <| n / 100
                _ -> k -- Better to have a Num type so I don't have to do this.

toggleSignOfNum : Arith -> Arith
toggleSignOfNum k =
            case k of
                Num n -> Num <| -1 * n
                _ -> k -- Better to have a Num type so I don't have to do this.



-- VIEW


calcButton : String -> Key -> Html Msg
calcButton value key =
    let widthClass =
        case key of
            NumKey 0 -> "w-50"
            _        -> "w-25"
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
                    calcButton "C" <| OpKey Clear
                ,   calcButton "+/-" <| OpKey ToggleSign
                ,   calcButton "%" <| OpKey Percent
                ,   calcButton "÷" <| OpKey Divide
                ,   calcButton "7" <| NumKey 7
                ,   calcButton "8" <| NumKey 8
                ,   calcButton "9" <| NumKey 9
                ,   calcButton "×" <| OpKey Times
                ,   calcButton "4" <| NumKey 4
                ,   calcButton "5" <| NumKey 5
                ,   calcButton "6" <| NumKey 6
                ,   calcButton "-" <| OpKey Minus
                ,   calcButton "1" <| NumKey 1
                ,   calcButton "2" <| NumKey 2
                ,   calcButton "3" <| NumKey 3
                ,   calcButton "+" <| OpKey Plus
                ,   calcButton "0" <| NumKey 0
                ,   calcButton "." <| DotKey
                ,   calcButton "=" <| OpKey Equal
                ]
        ]
