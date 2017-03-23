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
    , clearType : ClearType
    }

type ClearType = AC | C
type Unary = Equal | Percent | ToggleSign | Clear | AllClear
type Binary = Divide | Plus | Times | Minus
type Op = UnOp Unary | BinaryOp Binary

type Key = OpKey Op | NumKey Float | DotKey

type Arith =
    Op Op | Num Float | NumDot Float | ZeroDot | TempRes Float

type alias Buffer = List Arith

type alias KeyLog = List Key

init : (Model, Cmd Msg)
init =
    ({ keyLog = []
     , result = Nothing
     , buffer = []
     , clearType = AC
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
        result =
            case buffer of
                Op (UnOp Clear) :: _ -> Nothing
                _ -> Maybe.map display <| firstNum buffer
        clearType = case key of
                        OpKey (UnOp Clear) -> AC
                        NumKey _   -> C
                        _       -> model.clearType

    in { model1 | result = result, buffer = buffer, clearType = clearType }

firstNum : Buffer -> Maybe Arith
firstNum vals =
    let f next acc =
        case (acc, next) of
            (Just num, _) -> Just num
            (Nothing, Num n) -> Just <| Num n
            (Nothing, ZeroDot) -> Just <| ZeroDot
            (Nothing, NumDot n) -> Just <| NumDot n
            (Nothing, TempRes n) -> Just <| TempRes n
            _            -> Nothing
    in
    List.foldl f Nothing vals

-- f : Arith -> Maybe Arith -> Maybe Arith

display : Arith -> String
display val =
    case val of
        Num n       -> toString n
        ZeroDot     -> "0."
        NumDot n    -> toString n ++ "."
        TempRes n   -> toString n
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
    let buffer_ =
        case buffer of
            Op (UnOp Clear) :: restBuffer -> restBuffer
            _ -> buffer
    in
    case (key, buffer_) of
        (DotKey, bf)    ->
            case bf of
                []              -> [ZeroDot]
                Num x :: tl     -> NumDot x :: tl
                ZeroDot :: tl   -> ZeroDot :: tl
                NumDot x :: tl  -> NumDot x :: tl
                TempRes _ :: tl -> ZeroDot :: tl
                Op op :: tl     ->
                    case op of
                        UnOp Equal   -> [ZeroDot]
                        _               -> ZeroDot :: Op op :: tl
        (NumKey x, bf)  ->
            case bf of
                []              -> [Num x]
                TempRes _ :: tl -> Num x :: tl
                Num y :: tl     -> (concatNums (Num x) (Num y)) :: tl
                ZeroDot :: tl   -> (concatNums (Num x) ZeroDot) :: tl

                NumDot y :: tl  ->
                    (Num <| Result.withDefault 0
                         <| String.toFloat
                         <| toString y ++ "." ++ toString x) :: tl
                Op op :: Num y :: tl  ->
                    case op of
                        UnOp Equal -> [Num x]
                        _     -> Num x :: Op op :: Num y :: tl
                [Op _] ->
                    Debug.crash "Invalid State: Buffer cannot have an Op as last element."
                Op _ :: Op _ :: _ ->
                    Debug.crash "Invalid State: Two Ops cannot occur in sequence."
                Op _ :: NumDot _ :: _ ->
                    Debug.crash "Invalid State: A NumDot can only occur as first element of the buffer."
                Op _ :: TempRes _ :: _ ->
                    Debug.crash "Invalid State: A TempRes can only occur as first element of the buffer."
                Op _ :: ZeroDot :: _ ->
                    Debug.crash "Invalid State: A ZeroDot can only occur as first element of the buffer."
        (OpKey op , bf)  ->
            case op of
                UnOp AllClear -> []
                UnOp Clear ->
                    case bf of
                        []             -> []
                        Num x :: tl    -> Op (UnOp Clear) :: tl
                        NumDot _ :: tl -> Op (UnOp Clear) :: tl
                        ZeroDot :: tl  -> Op (UnOp Clear) :: tl
                        TempRes _ :: tl -> Op (UnOp Clear) :: tl
                        Op op :: tl    -> Op (UnOp Clear) :: Op op :: tl
                _ ->
                    case bf of
                        []                          -> []
                        [ZeroDot]                   -> eval [Op op, Num 0]
                        [Num x]                     -> eval [Op op, Num x]
                        [NumDot x]                  -> eval [Op op, Num x]
                        Op _ :: tl                  -> eval <| Op op :: tl
                        ZeroDot :: Op op_ :: Num x :: tl    ->
                            eval <| Op op :: Num 0 :: Op op_ :: Num x :: tl
                        NumDot y :: Op op_ :: Num x :: tl   ->
                            eval <| Op op :: Num y :: Op op_ :: Num x :: tl
                        TempRes y :: Op op_ :: Num x :: tl ->
                            eval <| Op op :: Num y :: Op op_ :: Num x :: tl
                        Num _ :: Op _ :: _          -> eval <| Op op :: bf
                        Num _ :: _                  -> Debug.crash "Invalid State: Nums can only be followed by an Op."
                        ZeroDot  :: _  -> Debug.crash "This ZeroDot form is invalid"
                        NumDot _  :: _  -> Debug.crash "This NumDot form is invalid"
                        TempRes _  :: _  -> Debug.crash "This TempRes form is invalid"

eval : Buffer -> Buffer
eval buffer =
    case buffer of
        [Op (UnOp op), Num x] ->
            case op of
                Percent     -> [Num (x / 100)]
                ToggleSign  -> [Num (-1 * x)]
                Equal       -> buffer
                Clear       -> Debug.crash "Shouldn't reach here. Clear should have been handled in earlier in the preceeding function."
                AllClear    -> Debug.crash "Shouldn't reach here. AllClear should have been handled in earlier in the preceeding function."
        [Op (BinaryOp _), _] -> buffer
        [Op op, Num y, Op op_, Num x] ->
            case op of
                (UnOp unOp) ->
                    case unOp of
                        -- Percent     -> [TempRes <| y / 100, Op op_, Num x]
                        Percent     -> [Num <| x * y / 100 , Op op_, Num x]
                        ToggleSign  -> [Num <| -1 * y, Op op_, Num x]
                        Equal       -> [Op op, calcBinary (Num y, Op op_, Num x)]
                        Clear       -> Debug.crash "Shouldn't reach here. Clear should have been handled in earlier in the preceeding function."
                        AllClear       -> Debug.crash "Shouldn't reach here. AllClear should have been handled in earlier in the preceeding function."
                (BinaryOp _) ->
                        if precedence op > precedence op_ then
                            buffer
                        else
                            [Op op, calcBinary (Num y, Op op_, Num x)]
        [Op op, Num z, Op op_, Num y, Op op__, Num x] ->
            [Op op, calcBinary (calcBinary (Num z, Op op_, Num y), Op op__, Num x)]
        _ -> Debug.crash "Buffer must start with an Op followed by a Num to be evaled."

precedence: Op -> Int
precedence op =
    case op of
        UnOp _ -> 100
        BinaryOp binOp ->
            case binOp of
                Divide  -> 10
                Plus    -> 5
                Times   -> 10
                Minus   -> 5

calcBinary : (Arith, Arith, Arith) -> Arith
calcBinary expr =
    Num <|
        case expr of
            (Num y, Op (BinaryOp Divide), Num x)   -> x / y
            (Num y, Op (BinaryOp Plus), Num x)     -> x + y
            (Num y, Op (BinaryOp Times), Num x)    -> x * y
            (Num y, Op (BinaryOp Minus), Num x)    -> x - y
            _                                      -> Debug.crash "Wrong structure."

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


-- VIEW


calcButton : String -> Key -> Html Msg
calcButton value key =
    let widthClass =
        case key of
            NumKey 0 -> "w-50"
            _        -> "w-25"
    in  button [ class <| "btn btn-success h-25 key rounded-0 " ++ widthClass
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
                <| List.map (labelToButton model)
                            [toString model.clearType, "+/-" ,"%" ,"÷" ,"7" ,"8" ,"9" ,"×" ,"4" ,"5" ,"6" ,"-" ,"1" ,"2" ,"3" ,"+" ,"0" ,"." , "="]
        ]

labelToButton : Model -> String -> Html Msg
labelToButton model label =
    calcButton label <| labelKey model label

labelKey : Model -> String -> Key
labelKey model label =
    case label of
        "C" -> OpKey <| UnOp Clear
        "AC" -> OpKey <| UnOp AllClear
        "+/-" -> OpKey (UnOp ToggleSign)
        "%" -> OpKey (UnOp Percent)
        "÷" -> OpKey (BinaryOp Divide)
        "7" -> NumKey 7
        "8" -> NumKey 8
        "9" -> NumKey 9
        "×" -> OpKey (BinaryOp Times)
        "4" -> NumKey 4
        "5" -> NumKey 5
        "6" -> NumKey 6
        "-" -> OpKey (BinaryOp Minus)
        "1" -> NumKey 1
        "2" -> NumKey 2
        "3" -> NumKey 3
        "+" -> OpKey (BinaryOp Plus)
        "0" -> NumKey 0
        "." -> DotKey
        "=" -> OpKey (UnOp Equal)
        _   -> Debug.crash "Not a valid label."
