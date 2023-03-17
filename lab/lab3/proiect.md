# Laboratorul 3 – Git repo + stack + parsec

In continuare vom face pasi catre organizarea codului scris pana acum intr-un proiect structurat. In particular vom:
- Crea un git repository pentru proiectul nostru
- Crea un proiect de Haskell folosind build tool-ul `stack`
- Integra dependinte externe ca `parsec` si `isocline` in proiect 

## Crearea unui proiect pe GitHub 

Creați-vă un cont pe GitHub :).

### Exercițiu (proiect nou)

Creați un proiect (repository) nou pe GitHub

Setați limbajul 

### Exercițiu (cheie SSH – opțional, pe calculatorul propriu)

Configurați-vă o cheie SSH pentru accesul GitHub.

[Cum sa generezi o cheie SSH](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)

Pentru a adauga o noua cheie in contul de GitHub, accesati meniul `Settings` -> `SSH and GPG Keys` -> `New SSH Key`.

### Exercițiu (copie locală a proiectului)

Faceți o copie locală (`clone`) a proiectului

Posibil să fie nevoie să vă instalați Git / GitHub for Windows

## Crearea unui proiect Haskell folosind `stack`

### Exercițiu (proiect nou)

Folosiți `stack` pentru a crea un proiect nou

### Exercițiu (dependințe parser)

Adăugați `parsec` la lista de dependințe a proiectului (`package.yaml`). `parsec` este o librarie de parsing care poate replica (si extinde) functionalitatea parserelor definite manual de voi in Laboratorul 2. 

### Exercițiu (dependințe readline)

O abilitate utilă pentru un REPL este aceea de a putea edita comanda introdusă la prompt si de a putea accesa istoricul acestor comenzi. Pentru a putea face acest lucru putem folosi o nouă bibliotecă. Din nou, puteți alege între mai multe alternative, dar laboratorul de azi e scris având în minte `isocline` deoarece este independentă de sistemul de operare și disponibilă pe `stackage`.

## Integrare cu parsec

Puteți folosi funcția [`makeTokenParser`](https://hackage.haskell.org/package/parsec-3.1.16.1/docs/Text-Parsec-Token.html#v:makeTokenParser) din biblioteca `parsec` pentru a obține un obiect de tip înregistrare
[GenTokenParser](https://hackage.haskell.org/package/parsec-3.1.16.1/docs/Text-Parsec-Token.html#t:GenTokenParser), prin care putem accesa combinatori de parsare similari tuturor celor definiți în laboratorul precedent.  

Funcția `makeTokenParser` ia ca argument un obiect de tipul [GenLanguageDef](https://hackage.haskell.org/package/parsec-3.1.16.1/docs/Text-Parsec-Token.html#t:GenLanguageDef), care descrie lucruri cum ar fi:

- cum sunt formate comentariile (`commentStart`, `commentEnd`, `commentLine`, `nestedComments`)
- cum sunt formați identificatorii (`identStart`, `identStart`) și operatorii (`opStart`, `opLetter`)
- cuvintele (`reservedNames`) și operatorii (`reservedOpNames`) cheie
- dacă limbajul face diferența între litere mari și mici (`caseSensitive`)

Putem să creem un obiect de felul acesta de la zero, sau putem particulariza unul din obiectele predefinite în modul [`Language`](https://hackage.haskell.org/package/parsec-3.1.16.1/docs/Text-Parsec-Language.html).
Deoarece limbajul nostru e bazat pe Haskell, putem începe cu
[`haskellStyle`](https://hackage.haskell.org/package/parsec-3.1.16.1/docs/Text-Parsec-Language.html#v:haskellStyle).

```{.hs .Parsing}
module Parsing where

import Exp
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
    ( haskellStyle, LanguageDef )
import Text.ParserCombinators.Parsec.Token
import Control.Applicative (some)
```

Definiți un obiect `LanguageDef` pentru `nanoHaskell` (bazat pe `haskellStyle`):

```{.hs .Parsing}
nanoHaskellDef :: LanguageDef st
miniHaskellDef = undefined
```

Folosind acesta, putem defini un parser pentru `nanoHaskell`:

```{.hs .Parsing}
nanoHs :: TokenParser st
nanoHs = makeTokenParser miniHaskellDef
```