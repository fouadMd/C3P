module.exports = $author$project$Jeu$serveurApi;

elm make Main.elm --output app.js

module Main exposing (..)

import Html exposing (..)


main =
  text publicApi.name


type alias PublicAPI =
    { name: String
    , greet : String -> String
    , sum : Int -> Int -> Int
    , tripleOr : Bool -> Bool -> Bool -> Bool
    , mavar : Montype
    , alter : Montype -> Montype
    }

type Montype =
  A | B Int


publicApi : PublicAPI
publicApi =
    { name = "my public api"
    , greet = greet
    , sum = sum
    , tripleOr = tripleOr
    , mavar = B 2
    , alter = alter
    }


alter : Montype -> Montype
alter a =
  case a of
    A -> B 0
    B x -> B (x + 1)


greet : String -> String
greet name =
    "Hello " ++ (double name) ++ "!"


double : String -> String
double name =
  name ++ name


sum : Int -> Int -> Int
sum x y =
    x + y




tripleOr : Bool -> Bool -> Bool -> Bool
tripleOr a b c =
    a || b || c
