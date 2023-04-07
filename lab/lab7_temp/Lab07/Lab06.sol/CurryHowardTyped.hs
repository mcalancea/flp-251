import Prelude ()

data False                                        -- empty type

data True = True                                  -- unit type

data And a b = And { proj1 :: a, proj2 :: b }     -- product

data Or a b                                       -- sum
  = Left a
  | Right b

type Not a = a -> False
type Iff a b = And (a -> b) (b -> a)

trueIntro :: True
trueIntro = True

falseElim :: False -> b
falseElim x = case x of

implElim :: (a -> b) -> a -> b
implElim f = f

implIntro :: (a -> b) -> a -> b
implIntro f = f

andIntro :: a -> b -> And a b
andIntro = And

andElimL :: And a b -> a
andElimL = proj1

andElimR :: And a b -> b
andElimR = proj2

orIntroL :: a -> Or a b
orIntroL = Left

orIntroR :: b -> Or a b
orIntroR = Right

orElim :: (a -> c) -> (b -> c) -> Or a b -> c
orElim fac fbc or
  = case or of
    Left a -> fac a
    Right b -> fbc b

notIntro :: (forall p. a -> p) -> Not a
notIntro f = f

notElim :: p -> Not p -> c
notElim p np = falseElim (np p)

iffIntro :: (a -> b) -> (b -> a) -> Iff a b
iffIntro = andIntro

iffElimL :: a -> Iff a b -> b
iffElimL a iff = andElimL iff a

iffElimR :: b -> Iff a b -> a
iffElimR b iff = andElimR iff b

pNPFalse :: p -> Not p -> False
pNPFalse = notElim

deMorgan1 :: And (Not p) (Not q) -> Not (Or p q)
deMorgan1 = implIntro (\anpnq -> orElim (andElimL anpnq) (andElimR anpnq))

deMorgan2 :: Not (Or p q) -> And (Not p) (Not q)
deMorgan2 = implIntro (\nOr -> andIntro (notIntro (\p -> notElim (orIntroL p) nOr)) (notIntro (\q -> notElim (orIntroR q) nOr)))

deMorgan3 :: Or (Not p) (Not q) -> Not (And p q)
deMorgan3 = implElim (\orNpNq -> notIntro (\andPQ -> falseElim (orElim (implIntro (\p -> notElim (andElimL andPQ) p)) (implIntro (\q -> notElim (andElimR andPQ) q)) orNpNq))) 

ax1 :: a -> b -> a
ax1 = implIntro (\a -> implIntro (\b -> a))

ax2 :: (a -> b) -> (a -> (b -> c)) -> a -> c
ax2 = implIntro (\f -> implIntro (\g -> implIntro (\a -> implElim (implElim g a) (implElim f a))))

ax3 :: a -> b -> And a b
ax3 = implIntro (\a -> implIntro (\b -> andIntro a b))

ax4 :: And a b -> a
ax4 = implIntro (\ab -> andElimL ab)

ax5 :: And a b -> b
ax5 = implIntro (\ab -> andElimR ab)

ax6 :: a -> Or a b
ax6 = implIntro (\a -> orIntroL a)

ax7 :: b -> Or a b
ax7 = implIntro (\b -> orIntroR b)

ax8 :: (a -> c) -> (b -> c) -> Or a b -> c
ax8 = implIntro (\ac -> implIntro (\bc -> implIntro (\ab -> orElim ac bc ab)))

ax9 :: (a -> b) -> (a -> Not b) -> Not a
ax9 = implIntro (\f -> implIntro (\g -> notIntro (\a -> notElim (implElim f a) (implElim g a))))

ax10 :: Not a -> a -> b
ax10 = implIntro (\na -> implIntro (\a -> notElim a na))

excludedMiddleImplDoubleNeg :: Or a (Not a) -> (Not (Not a) -> a)
excludedMiddleImplDoubleNeg =
  implIntro (\ana -> implIntro (\nna -> orElim (implIntro (\a -> a)) (\na -> notElim na nna) ana))
