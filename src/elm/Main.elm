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

type Key = OpKey Op | NumKey Int | DotKey

type Arith =
    Op Op | Num Float | NumDot String (Maybe String) | TempRes Float

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
            (Just num, _)           -> Just num
            (Nothing, Num n)        -> Just <| Num n
            -- (Nothing, ZeroDot)   -> Just <| ZeroDot
            (Nothing, NumDot a b)   -> Just <| NumDot a b
            (Nothing, TempRes n)    -> Just <| TempRes n
            _                       -> Nothing
    in List.foldl f Nothing vals

display : Arith -> String
display val =
    case val of
        Num n       -> toString n
        -- ZeroDot     -> "0."
        NumDot a b  -> a ++ "." ++ (Maybe.withDefault "" b)
        TempRes n   -> toString n
        Op _        -> Debug.crash "Only results can be displayed. Op cannot be a result."

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
                []              -> [NumDot "0" Nothing]
                Num x :: tl     -> NumDot (toString x) Nothing :: tl
                -- ZeroDot :: tl   -> ZeroDot :: tl
                NumDot x y :: tl  -> NumDot x y :: tl
                TempRes _ :: tl -> NumDot "0" Nothing :: tl
                Op op :: tl     ->
                    case op of
                        UnOp Equal  -> [NumDot "0" Nothing]
                        _           -> NumDot "0" Nothing :: Op op :: tl
        (NumKey x, bf)  ->
            case bf of
                []              -> [Num <| toFloat x]
                TempRes _ :: tl -> (Num <| toFloat x) :: tl
                Num y :: tl     ->
                    (Num <| Result.withDefault 0
                        <| String.toFloat
                        <| toString y ++ toString x) :: tl
                -- ZeroDot :: tl       -> NumDot 0 (Just x) :: tl
                NumDot a b :: tl    -> NumDot a (Just <| Maybe.withDefault "" b ++ toString x) :: tl
                Op op :: Num y :: tl  ->
                    case op of
                        UnOp Equal -> [Num <| toFloat x]
                        _     -> (Num <| toFloat x) :: Op op :: Num y :: tl
                [Op _] ->
                    Debug.crash "Invalid State: Buffer cannot have an Op as last element."
                Op _ :: Op _ :: _ ->
                    Debug.crash "Invalid State: Two Ops cannot occur in sequence."
                Op _ :: NumDot _ _ :: _ ->
                    Debug.crash "Invalid State: A NumDot can only occur as first element of the buffer."
                Op _ :: TempRes _ :: _ ->
                    Debug.crash "Invalid State: A TempRes can only occur as first element of the buffer."
                -- Op _ :: ZeroDot :: _ ->
                    -- Debug.crash "Invalid State: A ZeroDot can only occur as first element of the buffer."
        (OpKey op , bf)  ->
            case op of
                UnOp AllClear -> []
                UnOp Clear ->
                    case bf of
                        []             -> []
                        Num x :: tl    -> Op (UnOp Clear) :: tl
                        NumDot _ _ :: tl -> Op (UnOp Clear) :: tl
                        -- ZeroDot :: tl  -> Op (UnOp Clear) :: tl
                        TempRes _ :: tl -> Op (UnOp Clear) :: tl
                        Op op :: tl    -> Op (UnOp Clear) :: Op op :: tl
                _ ->
                    case bf of
                        []                          -> []
                        -- [ZeroDot]                   -> eval [Op op, Num 0]
                        [Num x]                     -> eval [Op op, Num x]
                        [NumDot a b]                ->
                            eval [Op op, numDotToNum <| NumDot a b]
                        Op _ :: tl                  -> eval <| Op op :: tl
                        -- ZeroDot :: Op op_ :: Num x :: tl    ->
                            -- eval <| Op op :: Num 0 :: Op op_ :: Num x :: tl
                        NumDot a b :: Op op_ :: Num x :: tl ->
                            eval <| Op op :: (numDotToNum <| NumDot a b) :: Op op_ :: Num x :: tl
                        TempRes y :: Op op_ :: Num x :: tl  ->
                            eval <| Op op :: Num y :: Op op_ :: Num x :: tl
                        Num _ :: Op _ :: _          -> eval <| Op op :: bf
                        Num _ :: _                  -> Debug.crash "Invalid State: Nums can only be followed by an Op."
                        -- ZeroDot  :: _  -> Debug.crash "This ZeroDot form is invalid"
                        NumDot _ _ :: _  -> Debug.crash "This NumDot form is invalid"
                        TempRes _  :: _  -> Debug.crash "This TempRes form is invalid"

numDotToNum :  Arith -> Arith
numDotToNum numDot =
    Num <|
    case numDot of
        NumDot a (Just b) ->
            case String.toFloat <| a ++ "." ++ b of
                Ok n -> n
                Err msg -> Debug.crash msg
        NumDot a Nothing ->
            case String.toFloat a of
                Ok n -> n
                Err msg -> Debug.crash msg
        _ -> Debug.crash "Only NumDots allowed here!"

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
                <| List.map labelToButton
                            [toString model.clearType, "+/-" ,"%" ,"÷" ,"7" ,"8" ,"9" ,"×" ,"4" ,"5" ,"6" ,"-" ,"1" ,"2" ,"3" ,"+" ,"0" ,"." , "="]
        ]

labelToButton : String -> Html Msg
labelToButton label =
    calcButton label <| labelKey label

labelKey : String -> Key
labelKey label =
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
