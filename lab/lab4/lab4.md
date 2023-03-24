# Laboratorul 4 – Evaluarea $\lambda$-termenilor

În acest laborator vom defini mașinăria de bază pentru evaluarea $\lambda$-termenilor:

- variabile și variabile libere
- variabile noi (față de o mulțime)
- $\alpha$-redenumirea variabilelor
- substituție
- $\beta$-reducție
- normalizare

## Reducerea expresiilor la expresii (mai) simple

După cum vă amintiți de la curs, pentru a evalua corect o expresie suntem uneori nevoiți să redenumim anumite variable. Considerați exemplul:

```
(\x -> \y -> x) y
```

unde substituția naivă a variabilei `x` cu variabila `y` duce la obtinerea functiei identitate:
```
(\y -> y)
```

cand in realitate ne dorim sa obtinem functia constanta care intoarce `y`:
```
(\y_1 -> y)
``` 

 > 📝 Nota: Numele exact al variabilei legate nu este important, este important doar sa nu fie `y`.

O abordare simpla este ca o variabilă să aibă pe lângă un nume și o valoare numerică care ne spune a câta variabilă cu acel nume este. In acest scop, definim un nou tip de date:

```{.hs .Exp}
data IndexedVar = IndexedVar
  { ivName :: String
  , ivCount :: Int
  } deriving (Eq, Read, Show)

makeIndexedVar :: String -> IndexedVar
makeIndexedVar name = IndexedVar name 0
```

Cu acest nou tip de variabile, putem defini un nou tip al $\lambda$-expresiilor, astfel: 

```{.hs .Exp}
data Exp
  = X IndexedVar
  | Lam IndexedVar Exp
  | App Exp Exp
  deriving (Show)
```

De ce sa adaugam acest tip daca este aproape identic cu `ComplexExp`? In laboratoarele urmatoare ne vom apropia mai mult de versiunea limbajului `miniHaskell` pe care am vazut-o in laboratorul 1: o versiune care suporta sintaxa pentru `List`, `let`, `if`, etc. In acel moment, vom extinde definitia tipului `ComplexExp` pentru a acomoda parsarea acestor noi constructii, dar tipul `Exp` va ramane la fel, fiindca "under the hood", la momentul evaluarii, toate sunt lambda-expresii "simple".

## Exercițiu (de la `Var` la `IndexedVar`)

```{.hs .Sugar}
module Sugar where

import Exp
```

Implementați (in fisierul `Sugar.hs`) o funcție care transformă un obiect de tip `Var` într-unul de tip
`IndexedVar` cu același nume și index `0`:

```{.hs .Sugar}
desugarVar :: Var -> IndexedVar
desugarVar = undefined

-- >>> desugarVar (Var "x")
-- IndexedVar {ivName = "x", ivCount = 0}
```

## Exercițiu (de la `IndexedVar` la `Var`)

Implementați o funcție care transformă un obiect de tip `IndexedVar` într-unul de tip `Var` (pentru a-l putea afișa). Pentru variabilele indexate cu un index diferit de `0`, să zicem `n`, folosiți `_n` ca prefix după numele variabilei.

```{.hs .Sugar}
sugarVar :: IndexedVar -> Var
sugarVar = undefined

-- >>> sugarVar (IndexedVar "x" 0)
-- Var {getVar = "x"}

-- >>> sugarVar (IndexedVar "x" 3)
-- Var {getVar = "x_3"}
```

Pentru transformarea expresiilor complexe în expresii simple vom folosi câteva
variabile predefinite:

### Exercițiu (de la `ComplexExp` la `Exp`)

Implementați următoarea funcție care transfomră un `ComplexExp` într-un `Exp`, astfel:

- `CApp`, `CLam` și `CX` se transformă în `App`, `Lam`, și respectiv `X`

```{.hs .Sugar}
desugarExp :: ComplexExp -> Exp
desugarExp = undefined
```

### Exercițiu (de la `Exp` la `ComplexExp`)

Implementați o funcție care traduce o expresie de tip `Exp` într-una de tip `ComplexExp`, traducând `App`, `Lam` și `X` în `CApp`, `CLam` și respectiv `CX`.

```{.hs .Sugar}
sugarExp :: Exp -> ComplexExp
sugarExp = undefined

-- >>> sugarExp (App (X (IndexedVar "x" 0)) (X (IndexedVar "y" 1)))
-- CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y_1"}))
```

## Variabile

```{.hs .Eval}
module Eval where

import Exp
import Data.List ( union, delete, nub )
```

### Exercițiu (variabile)

Definiți o funcție care dat fiind un $\lambda$-termen, calculează mulțimea (ca listă) variabilelor care apar în termen.

```{.hs .Eval}
vars :: Exp -> [IndexedVar]
vars = undefined

-- >>> vars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0}]
```

### Exercițiu (variabile libere)

Definiți o funcție care dat fiind un $\lambda$-termen, calculează mulțimea (ca listă) variabilelor libere care apar în termen.

```{.hs .Eval}
freeVars :: Exp -> [IndexedVar]
freeVars = undefined

-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]
```

Definiți o funcție care dată fiind o variabilă și un $\lambda$-termen, verifică dacă variabila apare liberă în termen.

```{.hs .Eval}
occursFree :: IndexedVar -> Exp -> Bool
occursFree = undefined

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False

-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True
```

### Exercițiu (variabilă nouă)

Scrieți o funcție care dată fiind o variabilă și o listă de variabile produce
o variabilă nouă care are același nume cu variabila dată dar e diferită de ea și de orice altă variabilă din lista dată.

```{.hs .Eval}
freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar = undefined

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}
```

## Substituții

### Exercițiu (redenumiri de variabile)

Scrieți o funcție care dată fiind o variabilă de redenumit, variabila care o redenumește și un $\lambda$-termen,
redenumește toate aparițiile variabilei de redenumit cu cea care o redenumește.

```{.hs .Eval}
renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement = undefined
```

### Exercițiu (substituție)

Scrieți o funcție care definește substituția unei variabile cu un termen într-un $\lambda$-termen, implementând
definiția matematică din curs (redenumind variabilele dacă există pericolul capturării).

```{.hs .Eval}
substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement = undefined
```

## Strategii de evaluare

### Exercițiu (normalizare)

Implementați strategia normală de beta-reducție.

```{.hs .Eval}
normalize :: Exp -> Exp
normalize = undefined

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})
```

## Intergrare cu REPL

### Exercițiu (REPL)

Importați modulele `Sugar` și `Eval` în `Main` și recrieți linia

```hs
              Right e -> putStrLn (showExp e) >> main
```

astfel încât să facă următoarele:

- să transforme expresia din `ComplexExp` în `Exp`
- să normalizeze expresia
- să transforme expresia înapoi în `ComplexExp`
- să afișeze expresia
- să ruleze din nou `main`