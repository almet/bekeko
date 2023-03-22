module Patterns exposing
    ( Pattern
    , PatternContent
    , PatternType(..)
    , alazani
    , availableChars
    , availablePatterns
    , dano
    , dartlo
    , fromString
    , koklata
    , makratela
    , shenako
    , toString
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


dartloStrlng =
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


makratelaString =
    """xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
              
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
              
              
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
              
              
              
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
              
              
              
              
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
              
              
              
              
              
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
              
              
              
              
              
              
xxxxxxxxxxxxxx
xxxxxxxxxxxxxx
              
              
              
              
              
              
              
xxxxxxxxxxxxxx
              
              
              
              
              
              
              
              """


koklataString =
    """oooooooooooo
/oo/oo/oo/oo
o//o//o//o//
oooooooooooo
ocooocooocoo
clcoclcoclco
ocooocooocoo
oooooooooooo
o//o//o//o//
/oo/oo/oo/oo
oooooooooooo

x x x x x x 
    
 l   l   l  
lll lll lll 
 l   l   l  
    
x x x x x x 

oooooooooooo
/o/o/o/o/o/o
oooooooooooo
occooccoocco
ooccooccoocc
oooooooooooo
/o/o/o/o/o/o
oooooooooooo

x x x x x x 
    
 l   l   l  
lll lll lll 
 l   l   l  
    
x x x x x x 

oooooooooooo
/o/o/o/o/o/o
oooooooooooo
occooccoocco
ooccooccoocc
oooooooooooo
/o/o/o/o/o/o
oooooooooooo

x x x x x x 
"""


dano : Pattern
dano =
    Pattern "Dano" (fromString danoString) Repeats


dartlo =
    Pattern "Dartlo" (fromString dartloStrlng) Repeats


shenako =
    Pattern "Shenako" (fromString shenakoString) Long


alazani =
    Pattern "Alazani" (fromString alazaniString) Repeats


makratela =
    Pattern "Makratela" (fromString makratelaString) Long


koklata =
    Pattern "Koklata" (fromString koklataString) Repeats


availablePatterns : List Pattern
availablePatterns =
    [ dano
    , dartlo
    , shenako
    , alazani
    , makratela
    , koklata
    ]


availableChars : List Char
availableChars =
    " xo/cl" |> String.toList


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


toString : PatternContent -> String
toString content =
    (++) "\u{000D}" (content |> List.foldl llne "" |> String.trimLeft)


llne : List Char -> String -> String
llne current acc =
    (++) acc (current |> String.fromList |> (++) "\u{000D}")


fromString : String -> PatternContent
fromString string =
    let
        llneLength =
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
        |> List.map (String.padRight llneLength ' ' >> String.toList)
