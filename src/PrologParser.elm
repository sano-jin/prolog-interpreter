module PrologParser exposing (..)

import Parser exposing (..)
import Char
import Set
import Util exposing (..)

type Term = Int Int
          | Var String
          | Fun String (List Term)

type alias Rule = (Term, List Term)
             
-- Lexer
lexeme : Parser a -> Parser a
lexeme p = p |. spaces 

nameLit : Parser String
nameLit =
    lexeme
    <| variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }

varLit : Parser Term
varLit =
    Parser.map Var
    <| lexeme
    <| variable
        { start = \c -> Char.isUpper c || c == '_'
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }
                   
intLit : Parser Term
intLit =
    Parser.map Int
    <| lexeme
    <| oneOf
        [ succeed negate
        |. lexeme (symbol "-")
        |= myInt
        , myInt
        ]

myInt : Parser Int
myInt =
    Parser.map (String.foldr (\c d -> d * 10 + Char.toCode c - 48) 0)
    <| getChompedString
    <| succeed ()
            |. chompIf Char.isDigit
            |. chompWhile Char.isDigit
    
-- Parser
sandwitched : String -> String -> Parser a -> Parser a
sandwitched left right p =
    succeed identity
        |. lexeme (symbol left)
        |= p
        |. lexeme (symbol right)

paren : Parser a -> Parser a
paren = sandwitched "(" ")"

sepBy1 : String -> Parser a -> Parser (List a)
sepBy1 sep p =
    Parser.map List.singleton p |> andThen (flip loop <| sepByHelp sep p)

sepBy : String -> Parser a -> Parser (List a)
sepBy sep p =
    oneOf [ sepBy1 sep p, succeed [] ]
        
sepByHelp : String -> Parser a -> List a -> Parser (Step (List a) (List a))
sepByHelp sep p as_ =
    oneOf
    [ succeed (\a -> Loop (a::as_))
    |. lexeme (symbol sep)
    |= p
    , succeed ()
    |> map (\_ -> Done (List.reverse as_))
    ]

sepByEnd : String -> Parser a -> Parser (List a)
sepByEnd sep p =
    loop [] <| sepByEndHelp sep p

sepByEndHelp : String -> Parser a -> List a -> Parser (Step (List a) (List a))
sepByEndHelp sep p as_ =
    oneOf
    [ p |> andThen (\a -> oneOf [ succeed (Loop (a::as_))
                                |. lexeme (symbol sep)
                                , succeed (Done <| List.reverse <| a::as_)
                                ]
                   )
    , succeed ()
    |> map (\_ -> Done (List.reverse as_))
    ]

    
list : Parser a -> Parser (List a)
list p =
    sandwitched "[" "]" <| sepBy "," p 
      
function : Parser Term
function =
    lexeme
    <| succeed Fun  
        |= nameLit
        |= oneOf [ paren <| sepBy "," <| lazy (\_ -> parseTerm)
                 , succeed []
                 ]

parseTerm : Parser Term
parseTerm = oneOf [ function, intLit, varLit ]

parseRule : Parser Rule
parseRule =
    succeed Tuple.pair
        |= parseTerm
        |= oneOf [ succeed identity
                 |. lexeme (symbol ":-")
                 |= sepBy "," parseTerm
                 , succeed []
                 ]
                    
parseRules : Parser (List Rule)
parseRules =
    sepByEnd "." parseRule
                                                
parser : Parser (List Rule)
parser =
    succeed identity
       |. spaces
       |= parseRules
       |. end 

-- show
showTerm : Term -> String
showTerm term =
    case term of
        Int i -> String.fromInt i
        Var v -> v
        Fun functor [] -> functor
        Fun functor args -> functor ++ "("
                               ++ String.join "," (List.map showTerm args)
                               ++ ")"

showRule : Rule -> String
showRule (head, body) = showTerm head
                        ++ if body == [] then ""
                           else ":- " 
                               ++ String.join "," (List.map showTerm body)

showRules : List Rule -> String
showRules rules = String.join "." (List.map showRule rules) ++ "."
  
problem2String : Problem -> String
problem2String problem = case problem of
                             Expecting str -> "expectiong " ++ str 
                             ExpectingInt -> "expecting int"
                             ExpectingNumber -> "expecting number"
                             ExpectingVariable -> "expecting variable"
                             ExpectingSymbol str -> "expecting symbol " ++ str
                             ExpectingKeyword str -> "expecting keyword" ++ str
                             ExpectingEnd -> "execting end"
                             UnexpectedChar -> "unexpected char"
                             Problem str -> "problem " ++ str
                             BadRepeat -> "badrepeat"
                             _ -> "error!"

