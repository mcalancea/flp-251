# Laboratorul 3 – Parsarea $\lambda$-expresiilor + REPL

In acest laborator vom scrie parsere pentru sintaxa $\lambda$-calcus-ului untyped si vom pune bazele unui mic Read-Eval-Print-Loop (REPL). Întrucât de partea de evaluare ne vom ocupa în laboratoarele următoare, astazi vom implementa doar un RPL :).

 > 📝 Nota: Interpretorul "miniHaskell" cu care am experimentat in Laboratorul 1 poate procesa diverse forme de "syntactic sugar": numere naturale, liste, perechi, etc. Fiindca nu am discutat inca metodele prin care aceste constructii sunt traduse in expresii $\lambda$, ne vom restrange deocamdata la a interpreta doar $\lambda$ calculul "pur". Un "nanoHaskell" daca doriti :). 

## Analiză sintactică pentru expresii

Fie tipul de date al expresiilor:
```{.hs .Exp}
module Exp where

newtype Var = Var { getVar :: String }
  deriving (Show)

data ComplexExp                         --  ComplexExp ::= "(" ComplexExp ")"
  = CX Var                              --          |   Var
  | CLam Var ComplexExp                 --          |   "\" Var "->" ComplexExp
  | CApp ComplexExp ComplexExp          --          |   ComplexExp ComplexExp
  deriving (Show)
```

Prin următoarele exerciții vom defini un parser care poate fi folosit pentru analiza sintactică a acestor expresii. În acest scop ne va ajuta biblioteca de parsare pe care ați scris-o în Laboratorul 2.

```{.hs .Parsing}
module Parsing where

import Exp
import Lab2
import Control.Applicative (some, many, (<|>))
import Data.Char (isAlpha, isAlphaNum)
```

Unele parsere pe care le vom folosi vor intoarce multiple parsari posibile. Pentru testare, vom folosi urmatoarea functie:

```{.hs .Parsing}
parseFirst :: Parser a -> String -> Maybe a
parseFirst p s
  = case apply p s of
      [] -> Nothing
      (a,_):_ -> Just a

```

 > 📝 Nota: Decizia de a selecta _prima_ parsare intr-o astfel de situatie este una euristica: desi functioneaza bine in practica, putem intalni in general cazuri in care avem nevoie de o alegere mai sofisticata. Pentru a-i spori eficacitatea, tratati cu atentie folosirea combinatorului alternativ `<|>`.


### Exercițiu (Identificatorii)

Definiți un parser care să accepte identificatorii care încep cu literă si continuă cu literă sau cifră.

```{.hs .Parsing}
var :: Parser Var
var = undefined
-- >>> parseFirst var "b is a var"
-- Just (Var {getVar = "b"})
```

Definiți un parser pentru variabile ca $\lambda$-expresii (folosiți `var`)

```{.hs .Parsing}
varExp :: Parser ComplexExp
varExp = undefined
-- >>> parseFirst varExp "b is a var"
-- Just (CX (Var {getVar = "b"}))
```

In continuare vom implementa parsere pentru fiecare tip de $\lambda$-expresie, respectiv parser-ul pentru o $\lambda$-expresie arbitrara (
`expr :: Parser ComplexExp`) care le imbina. Data fiind natura recursiva a $\lambda$-expresiilor, parserele particulare (spre exemplu, `lambdaExp`) vor apela recursiv parserul general `expr`. Pentru a putea testa intermediar aceste definitii, definiti initial `expr` ca fiind doar `varExp`.

```{.hs}
expr :: Parser ComplexExp
expr = varExp
```

### Exercițiu ($\lambda$-abstracții)

Folosind `expr` și `var` definiți un analizor sintactic care știe să
recunoască o $\lambda$-expresie de forma
`ComplexExp ::= "\" Var "->" ComplexExp`

```{.hs .Parsing}
lambdaExp :: Parser ComplexExp
lambdaExp = undefined
-- >>> parseFirst lambdaExp "\\x -> x"
-- Just (CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"})))
```

### Exercițiu (expresii parantezate)

```{.hs .Parsing}
parenExp :: Parser ComplexExp
parenExp = undefined
-- >>> parseFirst parenExp "(a)"
-- Just (CX (Var {getVar = "a"}))
```

### Exercițiu (Expresii de bază, fără aplicație)

O expresie de bază este una dintre urmatoarele:

- o $\lambda$-abstracție
- o variabilă (ca expresie)
- o expresie între paranteze

Scrieti un parser pentru o expresie de baza.

```{.hs .Parsing}
basicExp :: Parser ComplexExp
basicExp = undefined
-- >>> parseFirst basicExp "[a,b,c]"
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})])
```

### Toate expresiile (incluzând aplicația)

În sfârșit, o expresie este o succesiune de aplicații de expresii de bază. Fiindca tipul in `ComplexExp` aplicația este construită doar din două expresii, va fi nevoie sa construiti manual un arbore binar de aplicatii din aceasta succesiune de aplicatii.
Tineti cont ca aplicatia se asociaza la stanga.

Astfel, din șirul de intrare `"x y z t"` va trebui să obțineți
`CApp (CApp (CApp (Var "x) (Var "y)) (Var "z)) (Var "t)`

```{.hs .Parsing}
expr :: Parser ComplexExp
expr = varExp
-- >>> parseFirst expr "\\x -> x y z t"
-- Just (CLam (Var {getVar = "x"}) (CApp (CApp (CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y"}))) (CX (Var {getVar = "z"}))) (CX (Var {getVar = "t"}))))
```

Astfel am închis cercul și am obținut un parser pentru tipul $\lambda$-expresiilor. In final, pentru a obtine un comportament mai robust, avem grija sa:
- Eliminam whitespace-ul preliminar
- Acceptam doar parsarile care consuma complet sirul de intrare

```{.hs .Parsing}
exprParser :: Parser ComplexExp
exprParser = whiteSpace *> expr <* endOfInput
-- >>> parseFirst exprParser "let x := 28 in \\y -> + x y"
-- Just (Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"})))))
```

## Formatarea expresiilor

```{.hs .Printing}
module Printing (showExp) where

import Exp
```

### Exercițiu (formatare)

Implementați o funcție care formatează pentru afișare obiectele de tipul `ComplexExp`.

```{.hs .Printing}
showVar :: Var -> String
showVar = undefined

showExp :: ComplexExp -> String
showExp = undefined
```

## Interfața REPL (interacțiunea cu utilizatorul)

### Exercițiu (Parser pentru comenzi REPL)

```{.hs .REPLCommand }
module REPLCommand where

import Lab2
import Control.Applicative (many, (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String
```

Implementati un parser care dată fiind o comandă obține un obiect de tipul `REPLCommand`:

```{.hs .REPLCommand }
replCommand :: Parser REPLCommand
replCommand = undefined
```

Acest parser va trebui să înțeleagă următoarele comenzi:

- `:q` sau `:quit` pentru `Quit`
- `:l` sau `:load`, urmate de un șir de caractere pentru `Load`
- dacă nu e nici unul din cazurile de mai sus, tot șirul de intrare va fi pus într-un `Eval`.


### Exercițiu (programul principal)

Implementați repl-ul ca parte a funcției `main`.

- Afisează un prompt și citește o comandă
- parsează comanda într-un REPLComand
- în funcție de ea,
  - dacă e `Quit` termină programul
  - dacă e `Load`, deocamdată nu face nimic și reapelează `main`
  - dacă e `Eval`, atunci:
    - transformați șirul de intrare într-un obiect de tip expresie
    - transformați obiectul de tip expresie într-un șir de caractere prin formatare
    - afișați rezultatul
    - executați `main` din nou

```{.hs .Main}
module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand

main :: IO ()
main = undefined
```
