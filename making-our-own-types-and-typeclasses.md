---
theme: default
auto_scaling: true
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
Kunne ikke lagd funksjoner som så slik ut: `surface :: Circle -> Float` fordi
`Circle` er en value constructor. Kan ikke lage en signatur av type `False -> Float`
fordi `False` også er en value constructor (som ikke tar noen parametre)
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

Kunne også eksportert slik: `Shape`. Da ville konsumenter som importerer modulen
måtte bruke hjelpefunksjoner for å lage shapes.

`baseCircle` og `baseRect`

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

`a` er en type parameter. `Maybe` er en typekonstruktør. Kan være `Maybe Int`, `Maybe
Person` eller `Maybe String` osv. Kan aldri være typen `Maybe`.
List er også en type vi kjenner fra før som også bruker typekonstruktører.
&nbsp;

`Just 'a'` har typen `Maybe Char`

```hs
ghci> :t Nothing
Nothing :: Maybe a
```

Nothing er polymorfisk. Den kan oppføre seg som `Maybe Int` eller `Maybe String`
fordi den ikke returnerer noen verdi uansett. På samme måte som at `[]` er polymorfisk

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
} deriving (Eq)
```

<!-- Hvis vi ser på følgende datatype Person. Antar ingen personer har samme fornavn, etternavn og alder
    Gir mening å sjekke om to personer kan være den samme.

Nå kan vi sammenlikne to personer med == eller /=. Haskell sjekker så om verdikonstruktørene matcher, og så sjekker
den hvert par med felter med ==. Alle typene av felter må også være en del av Eq typeklassen.
    -->

```hs
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
ghci> mca == adRock
False
ghci> mikeD == adRock
False
ghci> mikeD == mikeD
True
ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
True
```

---
`Show` og `Read`

<!-- Er for typer som kan konverteres til og fra strings. Typene til feltene må også være
    en del av Show og Read-->

```hs
data Person = Person { firstName :: String
    , lastName :: String
    , age :: Int
} deriving (Eq, Show, Read)
```

Read:
<!-- Må eksplisitt sette typen når vi bruker read, hvis ikke vet ikke Haskell hvilken type resultatet skal være
    eller bruke resultatet på en måte som gjør at haskell kan infere typen.
    -->

```hs
ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}
```

Lese parameteriserte typer:
Kan ikke gjøre sånn: `"Just 't'" :: Maybe a`, men vi kan gjøre slik: `read "Just 't'" :: Maybe Char`.
<!-- Må spesifisere typeparameterene. -->
---

```hs
data Bool = False | True deriving (Ord)
```

`True` > `False` fordi `False` er spesifisert først.

<!-- Samme i Maybe. Nothing er spesifisert før just, så Nothing er mindre enn Just Something-->
<!-- Hvis vi sammenligner to Just-verdier, sammenlignes verdien inni disse -->

```hs
ghci> Nothing < Just 100
True
ghci> Nothing > Just (-49999)
False
ghci> Just 3 `compare` Just 2
GT
ghci> Just 100 > Just 50
True
```

---
`Enum` og `Bounded`

```hs
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

```hs
ghci> minBound :: Day
Monday
ghci> maxBound :: Day
Sunday
```

```hs
ghci> succ Monday
Tuesday
ghci> pred Saturday
Friday
ghci> [Thursday .. Sunday]
[Thursday,Friday,Saturday,Sunday]
ghci> [minBound .. maxBound] :: [Day]
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
```

<!-- Siden alle verdikonstruktørene er nullary (tar ingen parametere)
    kan vi gjøre typen en del av Enum typeklassen. Enum er for ting som tar forgjengere og etterfølgere

Bounded er for ting som har en min-verdi og en maks-verdi. Kan hente ut første og siste dag.

Enum gjør at vi kan lage ranges.

    -->
---

# Type synonyms

```hs
type String = [Char]
```

<!-- Gjør ingenting, bare gir noen typer forskjellige navn. -->

Disse er ekvivalente men sistnevnte er mer lesbar

```hs
toUpperString :: [Char] -> [Char]
toUpperString :: String -> String
```

---

Map som er en telefonbok:

```hs
phoneBook :: [(String,String)]
phoneBook =
[("betty","555-2938")
,("bonnie","452-2928")
,("penny","853-2492")
]
```

```hs
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]
```

<!-- Gjør koden mer lesbar. Typisk å gi strenger typesynonymer for å gi mer informasjon
    om hva strengene i funksjonen skal bli brukt som og hva de representerer.
    -->

```hs
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

inPhoneBook :: String -> String -> [(String,String)] -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook
```

<!-- Lar oss implementere denne funksjonen som har en deskriptiv typedeklarasjon.
    Burde ikke overdrive typesysnonymer-->

---

Typesynonymer kan også parameteriseres

```hs
type AssocList k v = [(k,v)]
```

<!-- Synonym for en association list. -->

Kan også bruke partial application til type parametre

```hs
type IntMap v = Map Int v
type IntMap = Map Int
```

<!-- IntMap tar 1 typeparameter, og det er hvilken type Int peker på. -->
---

```hs
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

<!-- Hvis Left er brukt, returneres data av typen a, hvis Right er brukt returneres innholdet av typen b
    Ofte brukt for feil når vi ønsker å formidle informasjon om hva som feilet.

Vi kan bruke Maybe, men Nothing sier ingenting om hva som feilet.

Typisk er da Left en feil og Right et resultat. Disse kan pattern-matches på i andre funksjoner.
    -->

```hs
import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
```

<!-- Eksempel: Har skap på en high school. Hvert skap har en kodelås
    Når en student vil ha et nytt skap, ber han om å få skap med nr x
    Så får de koden tilbake. Hvis noen allerede har skapet, får de ikke koden
    og de må velge et annet skap.
    -->

```hs
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
```

<!-- Her er feilen representert med en String, (Og resultatet, men vi har gitt den et type synonym Code) -->
---

```hs
lockers :: LockerMap
lockers = Map.fromList
[(100,(Taken,"ZD39I"))
,(101,(Free,"JAH3I"))
,(103,(Free,"IQSA9"))
,(105,(Free,"QOTSA"))
,(109,(Taken,"893JJ"))
,(110,(Taken,"99292"))
]

ghci> lockerLookup 101 lockers
Right "JAH3I"
ghci> lockerLookup 100 lockers
Left "Locker 100 is already taken!"
ghci> lockerLookup 102 lockers
Left "Locker number 102 doesn't exist!"
ghci> lockerLookup 110 lockers
Left "Locker 110 is already taken!"
ghci> lockerLookup 105 lockers
Right "QOTSA"
```

---

# Recursive data structures

<!-- Vi kan implementere vår egen liste!

    Det er enten en tom liste eller en kombinasjon av en head med en verdi
    og en liste.

Kaller Cons infix for å se at den oppfører seg likt.
    -->

Typekonstruktører kan referere til seg selv.

`[3,4,5,6]` er syntakssukker for `3:(4:(5:6:[]))` som også kan skrives slik `3:4:5:6:[]`

```hs
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```

```hs
ghci> Empty
Empty
ghci> 5 `Cons` Empty
Cons 5 Empty
ghci> 4 `Cons` (5 `Cons` Empty)
Cons 4 (Cons 5 Empty)
ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
Cons 3 (Cons 4 (Cons 5 Empty))
```

---
<!-- Kan definere funksjoner til å være automatisk infix ved å bruke spesialtegn. Kan også gjøre det samme
    med konstruktører.

Ny syntaktisk construct: fixity declaration.

r i infixr betyr at den er høyre-assossiativ og at fixity er 5, altså om grad av binding.

f.eks (*) er infixl 7 og (+) er infixl 6 Det betyr at gange binder tettere enn +
    -->

Fixity declarations:

```hs
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
```

```hs
ghci> 3 :-: 4 :-: 5 :-: Empty
(:-:) 3 ((:-:) 4 ((:-:) 5 Empty))
ghci> let a = 3 :-: 4 :-: 5 :-: Empty
ghci> 100 :-: a
(:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))
```

```hs
infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
```

<!-- Kan nå lage en kopi av `++` for vår egen listetype:
    Kaller den .++ men gjør akkurat det samme som ++ for vanlige lister
    Kunne implementer alle funksjoner på lister for vår egen listetype hvis vi ville.

Kan pattern matche x :-: xs fordi pattern matching egenlit matcher konstruktører.

    -->

```hs
ghci> let a = 3 :-: 4 :-: 5 :-: Empty
ghci> let b = 6 :-: 7 :-: Empty
ghci> a .++ b
(:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty))))
```

---

## Implementering av binærsøketre

![](binarytree.png)

<!-- et element som peker til to elementer. Det minste elementet til venstre og største til høyre.
    Hver av disse elementene kan peke til 0,1 eller to elementer.

Alle elementene i venstre subtre er mindre og alle elementer i høyre subtre er større.

    -->

---
<!-- Et tre er enten et tomt tree eller et element som har en verdi og to trær

Skal lage en funksjon som setter inn verdier i treet vårt.
Starter med en funksjon som lager et tree med en verdi og to tomme subtrær.
    -->

```hs
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
```
<!-- Hvis vi møte et EmptyTree så legger vi inn verdien vår i et singleton tre.

Ellers hvis lik, returner treet.
hvis mindre enn verdien vi står på, returner høyretree og bytt ut venstre tree.
hvis større enn verdien vi står på, returner venstre tree og bytt ut høyre tree.


 Så lager vi en funksjon som sjekker om et element finnes i treet vårt. -->
 ---
```hs
ghci> let nums = [8,6,4,1,7,3,5]
ghci> let numsTree = foldr treeInsert EmptyTree nums
ghci> numsTree
Node 5
    (Node 3
        (Node 1
            EmptyTree
            EmptyTree
        )
        (Node 4
            EmptyTree
            EmptyTree
        )
    )
    (Node 7
        (Node 6
            EmptyTree
            EmptyTree
        )
        (Node 8
            EmptyTree
            EmptyTree
        )
    )

```


---

# Typeclasses 102

<!-- En typeklasse definerer oppførsel, som f.eks sammenlikne likhet
    Og typer som kan oppføre seg på denne måten, blir gjort til instanser av typeklassen.

Oppførselen oppnåes ved å definere funksjoner eller typedeklarasjoner som vi implementerer.

Når vi sier at en type er en instans av en typeklasse, betyr det at vi kan bruke funksjonene
typeklassen definener på den typen.

Har ingenting med klasser å gjøre i imperative språk, så prøv å legg fra dokker imperative
klassekonsepter.

    -->

```hs
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

<!-- class bruker til å definere en ny typeklasse som heter Eq, a er typevariabelen. Må være et lowercase ord
    Må spesifisere typesignaturen til funkjsonene. Trenger ikke å definere implementasjonen.

Her er funksjoneimplementasjonen definert, og vi har definert de ved hjelp av mutual recursion.

Sagt at de er like dersom de ikke er forksjellige
og de er ulike dersom de ikke er like.
    -->

```hs
data TrafficLight = Red | Yellow | Green
```

<!-- Har ikke derived av Eq her. Det er fordi vi skal spesifisere de for hånd. -->

Sånn gjør vi den til en instans av `Eq`:

```hs
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
```

<!-- Her gjør vi det ved å bruke instance nøkkelordet. Class er for å lage typeklasser, og instance er for å
    gjøre typene våre til instanser av typeklassen.

    Byttet ut a-en med TrafficLight.

Fordi vi definerte (==) og (/=) med mutual recursion, så trenger vi bare å overskrive implementasjonen på 1 av disse
for å få begge til å fungere. Dette kallse minimal complete definition av en typeklasse.

Bruker pattern-matching til å definere likhet.
    -->
---

Show:

```hs
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

```hs
ghci> Red == Red
True
ghci> Red == Yellow
False
ghci> Red `elem` [Red, Yellow, Green]
True
ghci> [Red, Yellow, Green]
[Red light,Yellow light,Green light]
```

<!-- implementere også show med pattern-matching. Så kan vi se at de fungerer som de skal.
    Vi kunne bare brukt deriving for Eq og fått akkurat samme oppførsel.

    Men for Show måtte vi gjøre det på denne måten fordi vi ville customize oppførselen.
    -->

---

### Subklasser

Kan også lage typeklasser som er subklasser av andretypeklasser:

```hs
class (Eq a) => Num a where
    ...
```

<!-- Her er typeklassen num en subklasse av typeklassen Eq.
    Så en type må være en instans av Eq, før den kan være en instans av Num

    -->
---
Hva med `Maybe`?

```hs
instance Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

<!-- Maybe er ikke en konkret type, men en typekonstruktør som tar et type parameter.

Definerer derfor oppførselen for maybe til å sammenligne verdiene direkte for just-verdier.
Og spesifiserer samtidig at Nothing kan være lik Nothing.

Maybe er ikke en konkret type men det er Maybe a
    -->

Må forsikre oss om at m er en instans av Eq:

```hs
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

```

<!-- Så her bruker vi en begrensning i instansdeklarasjonen. Som betyr at vi vil at alle
    typer på formen Maybe m skal være del av Eq, men bare for typer av m som allerede er
    del av Eq.

-->
Kommando for å se informasjon om hvilke funksjoner en typeklasse definerer:
`:info YourTypeClass`

---

# A yes-no typeclass

<!--
    3 øverste vil alerte med NO, mens siste vil alerte med YEAH!
    Js behandler non-empty strings som en slags true-ish verdi -->


<!-- Typeklassen definerer 1 funksjon. Tar 1 verdi som hold på et konsept
    av true-ness og returnere Bool.

-->

```hs
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
```

<!-- id-funksjonen returnerer seg selv.  -->

<!-- Trenger ingen klassebegrensninger fordi vi ikke gjør noen antakelser om innholdet i maybe.
    Alle verdier som ikke er Nothing er True og Nothing er False.-->

---

```hs
ghci> yesno $ length []
False
ghci> yesno "haha"
True
ghci> yesno ""
False
ghci> yesno $ Just 0
True
ghci> yesno True
True
ghci> yesno EmptyTree
False
ghci> yesno []
False
ghci> yesno [0,0,0]
True
ghci> :t yesno
yesno :: (YesNo a) => a -> Bool

```

<!-- Ser at vi kan imitere Js sin bool-ish oppførsel med en slik typeklasse. -->

---

# The Functor typeclass

<!-- En typeklasse som i essens er for ting som kan mappes over. Nærliggende å tenke på lister
    og det stemmer. Lister er en del av Functor typeklassen.
    -->

```hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

<!-- definerer altså en funksjon, fmap og har ingen default implementasjon for denne.
    f er ikke en konkret type, men en typekonstruktør som tar ett typeparameter.
    -->

Ligner på map:

```hs
map :: (a -> b) -> [a] -> [b]
```

instansiert som en Functor slik:

```hs
instance Functor [] where
    fmap = map
```

<!-- map er bare en fmap, som fungerer KUN på lister.
    Ser fra hvordan lister er instansiert som en Functor, at vi sender inn en typekonstruktør []
    som tar imot nøyaktig ett typeparameter.
    -->

```hs
map :: (a -> b) -> [a] -> [b]
ghci> fmap (*2) [1..3]
[2,4,6]
ghci> map (*2) [1..3]
[2,4,6]
```

<!-- Får det samme resultatet av å bruke fmap og map på lister fordi de ER det samme. -->
---

### Typer som oppfører seg som en boks kan være Functors.

<!-- En liste kan være en boks med uendelig mange små rom
    som alle kan være tomme.
    -->

`Maybe a` er som en boks

<!-- som enten har ingenting aka Nothing eller nøyaktig en ting.  -->

```hs
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

<!-- Sånn er Maybe en functor. Igjen, så må Functor motta en typekonstruktør med ett parameter
    derfor skriver vi Maybe og ikke (Maybe m).
    -->
Mentalt kan man bytte ut `f` med `Maybe`:
Da blir `fmap` sånn: `(a -> b) -> Maybe a -> Maybe b`

```hs
ghci> fmap (*2) (Just 200)
Just 400
ghci> fmap (*2) Nothing
Nothing
```

<!-- Vi kan bruke map på andre ting en lister wohoo. -->
---

Kan gjøre `Tree a` til en `Functor`

<!-- Tree typen er en boks, fordi den holder flere eller ingen verdier, og typekonstruktøren tar
    nøyaktig ett typeparameter. -->

Hvis `fmap` var lagd kun for et `Tree`:
`(a -> b) -> Tree a -> Tree b`

```hs
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
```

<!-- EmptyTree mapper alltid til EmptyTree. Så applisere vi f til verdien i noden,
    og kjører fmap rekursivt på nodens subtrær, høyre og venstre. -->

---

Kan vi gjøre det samme for `Either`?

```hs
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```

<!-- Either er også en boks, men denne typekonstruktøren tar ikke ett typeparameter, men to.
    Men vi kan partially applye `Either` ved å gi den ett parameter, slik at det andre er
    fritt Functor (Either a).

Så hva skjer her? Vi mapper over Eithers høyre verdi, men vi gjør ingenting med dens venstre verdi,
fordi denne er av en annen type.
    -->

```hs
data Either a b = Left a | Right b
```

Kan ikke bruke én funksjon til å mappe over to verdier, når disse ikke er av samme type
&nbsp;

`Data.Map` kan også gjøres til en Functor fordi disse også holder verdier.

Hvis du har et map av typen `Map k v`, så vil `fmap` mappe en funksjon `v -> v'` over
et map av typen `Map k v` og returnere et map av typen `Map k v'`

---

Hvordan gjøre `Map k` til en instans av `Functor`:

```hs
import Data.Map (Map)
import qualified Data.Map as Map

class Functor' f where
fmap' :: (a -> b) -> f a -> f b

instance Functor' (Map k) where
fmap' f m = Map.fromDistinctAscList $ map (\(k, v) -> (k, f v)) $ Map.assocs m

ghci> let m = Map.fromList [(1, "one"), (2, "two")]
ghci> fmap' (++" Custom Functor instance") m
fromList [(1,"one Custom Functor instance"),(2,"two Custom Functor instance")]
```

<!-- Skrevet et forslag. Gjerne kom med bedre måter å gjøre det på, for her konvertere jeg til
    liste og bruker map og konverterer tilbake til Map. -->

---

# Kinds and some type-foo

Hvis du ikke skjønner dette, så er det ikke så farlig

<!-- Men hvis du skjønner det vil du få en god forståelse av haskells typesystem-->

---

<!-- Verdier har typer. Dem kan sees på som labels som gjør at vi kan tenke rundt verdiene.
    Men typer kan ha sine egne labels. Dette kalles Kinds.
    -->

## Kinds

```hs
ghci> :k Int
Int :: *
```

<!-- Stjerna betyr at typen er en konkret type. Altså en type som ikke tar noen parametre.
    kaller det bare star eller type.

    -->

&nbsp;

```hs
ghci> :k Maybe
Maybe :: * -> *
```
<!-- Maybe typekonstruktøren tar en konkret type som Int, og returnerer en konkret type Maybe Int
    Det er det stjerne -> stjerne betyr.

    Typer er labels for verdier og kinds er labels for typene.
    -->

```hs
ghci> :k Either
Either :: * -> * -> *
```

<!-- Dette betyr at Either tar 2 type parametre og produserer en konkret type.

Typekonstruktører er curried, så vi kan partially applie dem.
    -->


```hs
ghci> :k Either String
Either String :: * -> *
ghci> :k Either String Int
Either String Int :: *
```

<!-- Når vi gjorde Either til en del av Functor måtte vi partially applye, det er fordi
    Functor forventer typer av kind * -> * (tar et typeparameter)
    -->
