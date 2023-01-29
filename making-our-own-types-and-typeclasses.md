---
theme: default
class:
  invert
paginate: true
---

# Making Our Own Types And Typeclasses

---

# Algebraic data types intro

Definere nye datatyper:

```haskell
data Bool = False | True
```

`data` Definerer en ny datatype
`Bool` Navnet på typen vi definere
`False | True` Value constructors. Spesifiserer veridene denne typen kan ha

&nbsp;

`Int` er definert som:

```haskell
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
```

---

Definerer en type `Shape` som kan enten være en `Circle` eller `Rectangle`

```hs
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

&nbsp;

`Circle` En value constructor som har tre parametere som alle er av typen `Float`.

<!--
    Første to feltene er koordinatene til sirkelens senter,
    Tredje er sirkelens radius.
-->

`Rectangle` En value constructor som har fire parametere som alle er av typen `Float`
<!-- De to første floatene er koordinater til topleft og de to neste er koordinater til top right -->
&nbsp;

```hs
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape
ghci> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

---
Kan så lage en funksjon som regner ut en `Shape` sitt areal:

```hs
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

&nbsp;
Kunne ikke lagd funksjoner som så slik ut: `surface :: Circle -> Float` fordi `Circle` er en value constructor. Kan
heller ikke lage en signatur av type `False -> Float` fordi `False` også er en value constructor (som ikke tar noen
parametre)
<!-- Er dette vi har gjort når vi har pattern matchet tidligere i kapitlet. Når du pattern matcher på [] eller False eller tallet 5 har vi matchet på konstruktører, ikke typer-->

&nbsp;

```hs
ghci> surface $ Circle 10 20 10
314.15927
ghci> surface $ Rectangle 0 0 100 100
10000.0
```

---

Hvis vi prøver å printe `Circle 10 20 5` så får vi en error.

<!-- Dette er fordi haskell ikke vet hvordan den skal printe datatypen vår enda -->

```hs
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

ghci> Circle 10 20 5
Circle 10.0 20.0 5.0
ghci> Rectangle 50 230 60 90
Rectangle 50.0 230.0 60.0 90.0
```

<!-- Legger til deriving show, og nå er datatypen vår lagt til Show typeklassen, og Haskell kan printe datatypen vår-->

&nbsp;

Value constructors er funksjoner så vi kan delvis applisere de og mappe:

```hs
ghci> map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]
```

<!-- Lager 4 konsentriske sirkler med forskjellig radius -->
&nbsp;

Data typen vår kunne vært enda bedre om vi lagde en type for et punkt:

```hs
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

<!-- Dersom data typen har kun én konstruktør, er det vanling praksis å bruke samme navn på konstruktøren som på data typen-->
---

Må oppdatere funksjonen vår til å pattern matche den nye typen:

```hs
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

Kalles nå på denne måten:

```hs
ghci> surface (Rectangle (Point 0 0) (Point 100 100))
10000.0
ghci> surface (Circle (Point 0 0) 24)
1809.5574
```

---

### Eksportere data typer:

```hs
module Shapes
( Point(..)
, Shape(..)
, surface
) where
```

`(..)` betyr at vi også eksporterer value constructors for data typen.
Er det samme som å skrive `Shape (Rectangle, Circle)`

&nbsp;
Kunne også eksportert slik: `Shape`. Da ville konsumenter som importerer modulen måtte bruke hjelpefunksjoner for å lage
shapes. `baseCircle` og `baseRect`

```hs
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
```

<!-- Data.Map gjør dette da du ikke kan opprette et Map direkte, men du kan bruke en funksjon Data.Map.fromList til å opprette et Data.Map.Map -->

---

# Record syntax

```hs
data Person = Person String String Int Float String String deriving (Show)
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
ghci> guy
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
```

Funksjoner for å hente ut hver a propertiene til en `Person`

```hs
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname
```

---
Record:

```hs

data Person = Person { firstName :: String
    , lastName :: String
    , age :: Int
    , height :: Float
    , phoneNumber :: String
    , flavor :: String
} deriving (Show)
```

&nbsp;

Haskell lager automatisk funksjoner for å hente feltene på en person

```hs
ghci> :t flavor
flavor :: Person -> String
ghci> :t firstName
firstName :: Person -> String
```

```hs
ghci> Person {firstName="Buddy", lastName="Finkelstein", age=43, height=184.2, phoneNumber="526-2928", flavor="Chocolate"}
Person {firstName = "Buddy", lastName = "Finkelstein", age = 43, height = 184.2, phoneNumber = "526-2928", flavor = "Chocolate"}
```

<!-- Trenger ikke å spesifisere feltene i korrekt rekkefølge. Bruk records når det ikke er åpenbart hva feltene er
    Åpenbart hva feltene i Point er, men ikke i Person.
    -->
---

# Type parameters

Typekonstruktører kan motta typer som parametre for å produsere nye typer.

```hs
data Maybe a = Nothing | Just a
```

`a` er en type parameter. `Maybe` er en typekonstruktør. Kan være `Maybe Int`, `Maybe Person` eller `Maybe String` osv.
Kan aldri være typen `Maybe`.
List er også en type vi kjenner fra før som også bruker typekonstruktører.
&nbsp;

`Just 'a'` har typen `Maybe Char`

```hs
ghci> :t Nothing
Nothing :: Maybe a
```

Nothing er polymorfisk. Den kan oppføre seg som `Maybe Int` eller `Maybe String` fordi den ikke returnerer noen verdi
uansett. På samme måte som at `[]` er polymorfisk

---
<!--
Lønner seg å bruke type parametre når data typen vår vil fungere uansett hvilken type verdien den holder innehar.
Hvis typen vår oppfører seg som en slags boks, er det :thumbsup:
    -->

Map er en parameterisert type vi har sett på tidligere

```hs
data (Ord k) => Map k v = ...
```

<!--
Vi kunne lagt til en typeklassebegrensning på key-en, men det er en sterk konvensjon at vi ikke gjør dette i Haskell.
Dette er fordi at vi må putte typeklassebegrensningen på alle funksjoner som forventer at `k` i `Map` skal være
sorterbar. Dette må gjøres uansett om data deklarasjonen allerede har begrensningen eller ikke

Ikke put typeklassebegrensninger i datadeklarasjoner, fordi du må uansett putte dem på funksjontypedeklarasjonen
  -->

---

Implementasjon av en vektortype:

```hs
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
```

<!--
    Parameteriserer typen, fordi den skal støtte flere typer numbers.

Merk at vi ikke legger en begrensning på typekonstruktøren.
    -->
---

# Derived instances

<!-- Typeklasser som vi har snakket om tidligere er en slags interface som definerer oppførsel.
    Hvis datatypen vår oppfører seg som noe som kan sammenlignes, gjør vi den til en instans av Eq.
    -->

Haskell kan automatisk gjøre typene våre til instanser av disse typeklassene
`Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read`.

```hs
data Person = Person { firstName :: String
    , lastName :: String
    , age :: Int
}
```

<!-- Hvis vi ser på følgende datatype Person. Antar ingen personer har samme fornavn, etternavn og alder
    Gir mening å sjekke om to personer kan være den samme.
    -->

---

# Type synonyms

---

# Recursive data structures

---

# Typeclasses 102

---

# A yes-no typeclass

---

# The Functor typeclass

---

# Kinds and some type-foo