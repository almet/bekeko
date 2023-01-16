module Patterns exposing
    ( Pattern
    , PatternContent
    , PatternType(..)
    , alazani
    , availablePatterns
    , dano
    , dartlo
    , shenako
    )

import Maybe exposing (withDefault)


danoString : String
danoString =
    """
xx xx xx xx 
  x     x
xx xx xx xx 

o o o o o o 
 o o o o o o
o o o o o o 

xx xx xx xx 
  x     x
xx xx xx xx 

o o o o o o 
 o o o o o o
o o o o o o """


dartloString =
    """
xxxxxxxxxxxx
 x x x x x x
xxxxxxxxxxxx

/// /// /// 
 /   /   /  
/// /// /// 

xxxxxxxxxxxx
 x x x x x x
xxxxxxxxxxxx

oo o o oo o 
   o o    o 
oo o o oo o """


shenakoString =
    """

o o o o o o o o o

//////////////////
/ / / / / / / / /
   /   /   /   /

 c c   c c   c c  
cc cc cc cc cc cc
  l     l     l
cc cc cc cc cc cc
 c c   c c   c c 

   /   /   /   /
/ / / / / / / / /
//////////////////

o o o o o o o o o 

 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x
xxx xxx xxx xxx xx
 x   x   x   x   x

"""


alazaniString =
    """  x   x   x 
 xxx xxx xxx
xxoxxxoxxxox
xoooxoooxooo
oo/ooo/ooo/o
o///o///o///
// /// /// /
/   /   /   
  x   x   x 
 xxx xxx xxx
xxoxxxoxxxox
xoooxoooxooo
oo/ooo/ooo/o
o///o///o///
// /// /// /
/   /   /   """


dano : Pattern
dano =
    Pattern "Dano" (fromString danoString) Repeats


dartlo =
    Pattern "Dartlo" (fromString dartloString) Repeats


shenako =
    Pattern "Shenako" (fromString shenakoString) Long


alazani =
    Pattern "Alazani" (fromString alazaniString) Repeats


availablePatterns : List Pattern
availablePatterns =
    [ dano
    , dartlo
    , shenako
    , alazani
    ]


type alias Pattern =
    { name : String
    , content : PatternContent
    , type_ : PatternType
    }


type PatternType
    = Repeats
    | Long


type alias PatternContent =
    List (List Char)


fromString : String -> PatternContent
fromString string =
    let
        lineLength =
            string
                |> String.lines
                |> List.map String.length
                |> List.sort
                |> List.reverse
                |> List.head
                |> withDefault 0
    in
    string
        |> String.lines
        |> List.map (String.padRight lineLength ' ' >> String.toList)
