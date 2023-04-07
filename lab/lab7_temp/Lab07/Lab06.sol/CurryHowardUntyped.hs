import Prelude ()

newtype False = False { getVoid :: forall t. t }
newtype True = True { getTrue :: forall t . t -> t }
newtype And a b = And { getAnd :: forall t. (a -> b -> t) -> t }
newtype Or a b = Or { getOr :: forall t . (a -> t) -> (b -> t) -> t}
type Not a = a -> False
type Iff a b = And (a -> b) (b -> a)

trueIntro :: True                                   -- true introduction
trueIntro = True (\x -> x)

falseElim :: False -> b                             -- false elimination
falseElim x = getVoid x

implIntro :: (a -> b) -> (a -> b)                   -- implication introduction
implIntro = \a -> a

implElim :: (a -> b) -> a -> b                      -- implication elimination
implElim = \a -> a

andIntro :: a -> b -> And a b                       -- and introduction
andIntro a b = And (\f -> f a b)

andElimL :: And a b -> a                            -- and elimination 1
andElimL and = getAnd and (\x y -> x)

andElimR :: And a b -> b                            -- and elimination 2
andElimR and = getAnd and (\x y -> y)

orIntroL :: a -> Or a b                             -- or introduction 1
orIntroL x = Or (\l r -> l x)

orIntroR :: b -> Or a b                             -- or introduction 2
orIntroR y = Or (\l r -> r y)

orElim :: Or a b -> (a -> c) -> (b -> c) -> c       -- or elimination
orElim or = getOr or

notElim :: Not p -> p -> c                          -- not elimination 
notElim np p = falseElim (np p)

notIntro :: (forall p. a -> p) -> Not a             -- not introduction
notIntro f = f

iffIntro :: (a -> b) -> (b -> a) -> Iff a b         -- iff introduction
iffIntro = andIntro

iffElimL :: Iff a b -> a -> b                       -- iff elimination 1
iffElimL = andElimL

iffElimR :: Iff a b -> b -> a                       -- iff elimination 1
iffElimR = andElimR

pNPFalse :: p -> Not p -> False
pNPFalse p np = notElim np p

falseNotTrue :: False -> Not True
falseNotTrue f _ = f 

notTrueFalse :: Not True -> False
notTrueFalse f = f trueIntro

iffFalseNotTrue :: Iff False (Not True)
iffFalseNotTrue = andIntro falseNotTrue notTrueFalse

deMorgan1 :: And (Not p) (Not q) -> Not (Or p q)
deMorgan1 anpnq = notIntro (\or -> orElim or (notElim (andElimL anpnq)) (notElim (andElimR anpnq)))

deMorgan2 :: Not (Or p q) -> And (Not p) (Not q)
deMorgan2 nOr = andIntro (\p -> nOr (orIntroL p)) (\q -> nOr (orIntroR q))

deMorgan3 :: Or (Not p) (Not q) -> Not (And p q)
deMorgan3 orNpNQ andPQ = orElim orNpNQ  (\np -> notElim np p) (\nq -> notElim nq q)
  where
    p = andElimL andPQ
    q = andElimR andPQ

excludedMiddleImplDoubleNeg :: Or a (Not a) -> (Not (Not a) -> a)
excludedMiddleImplDoubleNeg =
  implIntro (\ana -> implIntro (\nna -> orElim ana (implIntro (\a -> a)) (\na -> notElim nna na)))


doubleNegImplExcludedMiddle :: (forall a. Not (Not a) -> a) -> Or b (Not b)
doubleNegImplExcludedMiddle = 
  \dNeg ->
    implElim
      dNeg
      (notIntro
        (\hNor ->
            notElim
              (notIntro
                (\nb ->
                    notElim
                      hNor
                      (orIntroR nb)
                )
              )
              (notIntro
                (\b ->
                    notElim
                      hNor
                      (orIntroL b)
                )
              )
        )
      )
