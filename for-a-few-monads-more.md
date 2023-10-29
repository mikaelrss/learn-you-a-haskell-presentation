---
theme: default
auto_scaling: true
class: invert
paginate: true
---

# For a Few Monads More

---

# Writer monad


```hs
isBigGang :: Int -> Bool
isBigGang x = x > 9
```
<!--
    Har en funksjon isBigGang
-->
---

# Writer monad



```hs
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")
```
<!--
    Vi endrer den til å ha med seg en log-verdi (en konteks).
-->

---

# Writer monad

```hs
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")
```


```
ghci> isBigGang 4
(False,"Compared gang to 9")
```

<!--
    Returnerer verdien med en logg som sier hva som skjedde inni funksjonen.
-->
---

# Writer monad
<!--
    Hva hvis vi vil gi en verdi med kontekst som input til funkjsonen?
    Samme problemet som vi hadde i forrige kapittel. Hvordan tar vi en verdi med kontekst
    og pakker ut verdien og feeder denne til funksjonen vår.
-->

## applyLog

```hs
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
```
---

# Writer monad

## applyLog

```hs
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
```
```
ghci> applyLog (3, "Smallish gang. ") isBigGang
(False,"Smallish gang. Compared gang to 9")
```
<!--
    Ser her at den nye log-verdien blir lagt til den originale log-verdien.
-->
---

# Writer monad

## applyLog

```hs
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
```
```
ghci> applyLog (3, "Smallish gang. ") isBigGang
(False,"Smallish gang. Compared gang to 9")
```
```
ghci> applyLog ("Tobin", "Got outlaw name.") (\x -> (length x, "Applied length."))
(5,"Got outlaw name.Applied length.")
```

<!--
    Enda et eksempel med en lambda. Merk at x kun er verdien uten kontekst. 
-->

---

# Writer monad

## Monoids to the rescue

<!--
    applyLog tar inn en verdi med kontekst String, men må dette være en string?
    ++ gjør at vi kunne brukt List.
-->
```hs
applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
```

---

# Writer monad

## Monoids to the rescue

<!--
    Monoids har mappend. Det betyr at vi kan gjøre denne funksjonen for alle monoids.
    Er ikke lenger en verdi med en log, det er en verdi med en Monoid kontekst. 
-->
```hs
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, mappend log newLog)
```

---

# Writer monad

## Monoids to the rescue

<!--
    Nå trenger ikke den tillagte verdien å være en log lenger.
    Lager en funksjon som legger til drikke til et måltid.
    
    Hvis bønner -> Melk
    Hvis Jerky -> Whisky
    eller -> øl
-->

```hs
import Data.Monoid  

type Food = String  
type Price = Sum Int  

addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)  
```

---

# Writer monad

## Monoids to the rescue

<!--
    Lager en funksjon som legger til drikke til et måltid.
    
    Hvis bønner -> Melk
    Hvis Jerky -> Whisky
    eller -> øl
-->

```hs
import Data.Monoid  

type Food = String  
type Price = Sum Int  

addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)  
```

```
ghci> ("beans", Sum 10) `applyLog` addDrink  
("milk",Sum {getSum = 35})  
ghci> ("jerky", Sum 25) `applyLog` addDrink  
("whiskey",Sum {getSum = 124})  
ghci> ("dogmeat", Sum 5) `applyLog` addDrink  
("beer",Sum {getSum = 35})  
```
<!--
    Starter med et måltid og legger til drikker. Den totale prisen blir vedien i kontekst.
    Tydelig at hvordan verdien i en sånn kontekst legges sammen avhenger av monoidens mappend.
-->

---

# Writer monad

## Writer type

```hs
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```
<!--
    Ser at bind er samme implementasjon som applyLog, bare at man wrapper ut Writer typen
    på en litt annen måte  en vår tuple.
-->


---

# Writer monad

## Writer type

```hs
instance (Monoid w) => Monad (Writer w) where  
return x = Writer (x, mempty)  
```
```
ghci> runWriter (return 3 :: Writer String Int)  
(3,"")  
ghci> runWriter (return 3 :: Writer (Sum Int) Int)  
(3,Sum {getSum = 0})  
ghci> runWriter (return 3 :: Writer (Product Int) Int)  
(3,Product {getProduct = 1})
```
<!--
    Return tar en verdi og putter den i en default minimal kontekst
    
    Return: runWriter wrapper writeren i en tuple. Må gjøres fordi writer ikke har en
    Show instans. 
-->


---

# Writer monad

## Do notation

```hs
import Control.Monad.Writer  

logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)
```

<!--
    Et eksempel på bruk av Writer og do notation.
    
    logNumber gjør om en int til en writer. Legger på en log-verdi.
-->


---

# Writer monad

## Writer type

```hs
import Control.Monad.Writer  

logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)
```

```
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5"])
```

<!--
    returnerer resultatet, og logger for hvert steg.
-->


---

# Writer monad

## Do notation

```hs
import Control.Monad.Writer  

logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)
```

```
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5","Gonna multiply these two"])  
```

<!--
    Kan bruke tell funksjonen til å logge ekstra informasjon.
-->



---

# Writer monad

## Adding logging to programs

```hs
gcd' :: Int -> Int -> Int  
gcd' a b   
    | b == 0    = a  
    | otherwise = gcd' b (a `mod` b)  
```

```
ghci> gcd' 8 3  
1  
```

<!--
    Euclids algoritme for å finn greatest common divisor.
-->


---

# Writer monad

## Adding logging to programs

```hs
import Control.Monad.Writer  

gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b) 
```


<!--
    Kan gjøre om slik at resultatet bringer med seg en kontekst. 
-->

---

# Writer monad

## Adding logging to programs

```hs
import Control.Monad.Writer  

gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b) 
```

```
ghci> fst $ runWriter (gcd' 8 3)  
1   
```

<!--
    første "plass" i tupelen er resultatet
-->


---

# Writer monad

## Adding logging to programs

```hs
import Control.Monad.Writer  

gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b) 
```

```
ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)  
8 mod 3 = 2  
3 mod 2 = 1  
2 mod 1 = 0  
Finished with 1  
```

<!--
    Endret originale algoritmen til en som rapporterer hva som skjer for oss ved å endre
    til å bruke monadiske verdier. Rapporteringen håndteres av >>=
-->


---

# Writer monad

## Inefficient List Construction


<!--
    Må tenke oss om hvilke monoider vi bruker.
    Lister bruker ++ for mappend, og det kan være langsomt for store lister.
    
    Er rask i gdc fordi den ser ut som denne:
    
    Lister konstrueres fra venstre til høyre. Dette er effektivt.
-->

```hs
a ++ (b ++ (c ++ (d ++ (e ++ f))))  
```

---

# Writer monad

## Inefficient List Construction


<!--
    Kan ende opp med listekonstruksjon som ser ut som dette.
    Er ineffektivt fordi hver gang noe må legges til på hyre side, må
    hele listen som er til venstre konstrueres på nytt. 
-->

```hs
((((a ++ b) ++ c) ++ d) ++ e) ++ f  
```

---

# Writer monad

## Inefficient List Construction


<!--
    Denne implementasjonen konstruere listen i revers.
    Fordi vi kan ende opp med å lage en slik ineffektiv liste,
    er det alltid lurt å bruke en datastruktur som støtter effektiv
    appending.
-->

```hs
import Control.Monad.Writer  

gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result  
```

```
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)  
Finished with 1  
2 mod 1 = 0  
3 mod 2 = 1  
8 mod 3 = 2   
```


---

# Writer monad

## Difference List


<!--
    Er en funksjon som tar en liste og prepender en annen list til den.
    Så en differnece list ekvivalent til [1,2,3] ser sånn ut. 
-->

```hs
\xs -> [1,2,3] ++ xs
```
---

# Writer monad

## Difference List

<!--
    appending kan gjøres på denne måten
-->

```hs
f `append` g = \xs -> f (g xs)  
```

```hs
f = ("dog"++)
g = ("meat"++)
```

```hs
\xs -> "dog" ++ ("meat" ++ xs)  
```


---

# Writer monad

## Difference List

<!--
    Kan lage en ny type wrapper for diffliste
-->

```hs
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  
```

---

# Writer monad

## Difference List

<!--
    Kan enkelt konvertere fra diffListe og til diffliste
-->

```hs
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  
```

```hs
toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  

fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f [] 
```


---

# Writer monad

## Difference List

<!--
    Mappend er bare funksjonskomposisjon, og mempty er det samme som identity funksjonen
-->


```hs
instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  
```

---

# Writer monad

## Difference List

<!--
    Og vi kan se at det fungerer som det skal
-->


```hs
instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  
```

```hs
fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])  
[1,2,3,4,1,2,3]  
```


---

# Writer monad

## Difference List

<!--
    Kan bruke difflist til å implemenetere en raskere versjon av revers logging på gcd'
    
    Bytter bare monoiden fra String til Difflist. og konvertere log-listen til en diffList med toDiffList
-->


```hs
import Control.Monad.Writer  

gcd' :: Int -> Int -> Writer (DiffList String) Int  
gcd' a b  
    | b == 0 = do  
        tell (toDiffList ["Finished with " ++ show a])  
        return a  
    | otherwise = do  
        result <- gcd' b (a `mod` b)  
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
        return result  
```

---

# Writer monad

## Difference List

<!--
    Kan se at det fungerer
-->


```
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34  
Finished with 2  
8 mod 2 = 0  
34 mod 8 = 2  
110 mod 34 = 8  
```


---

# Writer monad

## Comparing performance

<!--
    En funksjon som teller ned fra et tall og produserer loggen i revers
    Hvis du kjører dette vil den skrive ut en liste med alle tall fra 0 og opp til input
-->


```hs
finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])  

```

---

# Writer monad

## Comparing performance

<!--
    Her er den samme funksjonen implementert som en liste 
    Kjør demo og vis hvor stor forskjell det er.
-->


```hs
finalCountDown :: Int -> Writer [String] ()  
finalCountDown 0 = do  
    tell ["0"]  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell [show x]  

```








---

# Reader

## Functions as monads

<!--
    Funksjoner kan ogå være monader
    
    Dette er mond instansen for funksjon
    
    bind resultatet er en funksjon.
-->

```hs
instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w
```


---

# Reader

## The reader monad

<!--
    Her er en do-eksempel som bruker denne moanden
-->

```hs
import Control.Monad.Instances  

addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)  
```


---

# Reader

## The reader monad

<!--
    Her er en do-eksempel som bruker denne monaden.
    
    Både (*2) og (+10) blir applied til argumentet (3)
    også return (a+b) blir det. Men den ignorerer inputen
    og returnerer (a+b).
    
    Derfor blir denne kalt reader monaden.
    
    Alle funksjonene leser fra en felles kilde.
-->

```hs
import Control.Monad.Instances  

addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)  
```

```
ghci> addStuff 3  
19  
```
---

# Reader

## The reader monad

<!--
    Hvis vi skriver om til sånn ser vi at reader monaden
    lar oss behandle funksjoner som verdier med context.
    
    We can oppføre oss som om vi vet hva funksjonene skal
    returnere.
    
    Hvis vi har et sett med funksjoner som bare tar ett parameter
    og som skal appliseres på samme ting, kan vi bruke
    reader monaden til å ekstrahere resultatet deres og
    >>= vil fikse det.
-->

```hs
addStuff :: Int -> Int  
addStuff x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b  
```

---
<!--
    Fra kapittel 9 genererte vi tilfeldige tall.
    
    Tar imot en generator og genrerer et resultat basert
    på den. Returnere resultat og genrerer en ny verdi osv...
    -->
 
# Tasteful Stateful Computations

```hs
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  
```

---
<!--
    Implementerer en stack
    
    push/pop
    
    -->
 
# Stateful Computations
## Stack

```hs
type Stack = [Int]  

pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  

push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  
```

---
<!--
    Tar en stack. Pusher 3 og popper to ganger.
    -->
 
# Stateful Computations
## Stack

```hs
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2 
```


---
<!--
    resultatet bli 5 og resten av stacken.
    
    Vi har tatt noen komputasjoner og limt de sammen.
    
    Høres kjent ut.
    Kjedelig å skrive, siden vi manuelt assigner resultatet fra forrige komputasjon til neste.
    
    
    -->
 
# Stateful Computations
## Stack

```hs
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2 
```

```
ghci> stackManip [5,8,2,1]  
(5,[8,2,1])
```

---
<!--
    kulere om vi kunne skrevet sånn
    -->
 
# Stateful Computations
## Stack

```hs
stackManip = do  
    push 3  
    a <- pop  
    pop  
```



---
<!--
    Control.Monad.State innehoder en newtype som wrapper statefulle komputasjoner.
    
    Manipulerer en statae av typen s og har et resultat av typen a.
    
    Return presenterer en verdi og holder staten uendret
    
    bind >>= resultatet må være en stateful computation. så alt wrappes i State.
    Så en lamba som er vår statefulle komputasjon. Henter ut verdi a fra første
    statefulle komputasjon h ved å gi h vår nåværende  tilstand s. Resulterer i en
    tuple som vi destrukturerer verdien fra. Så appliserer  vi f på a som resulterer i
    en ny stateful komputasjon. Vi har ny komputasjon og ny state. Appliserer så
    komputasjonen på state. 
    -->
 
# Stateful Computations
## Stack

```hs
newtype State s a = State { runState :: s -> (a,s) }  
```
```hs
instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState
```


---
<!--
    Sånn kan vi wrappe pop og push i en State wrapper:
    -->
 
# Stateful Computations
## Stack

```hs
import Control.Monad.State  

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)  
```





---
<!--
    Nå kan vi skrive om eksempelet på denne formen.
    -->
 
# Stateful Computations
## Stack

```hs
import Control.Monad.State  

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop    
```


---
<!--
    Vil gi dette resultatet
    -->
  
# Stateful Computations
## Stack

```hs
import Control.Monad.State  

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop    
```

```
ghci> runState stackManip [5,8,2,1]  
(5,[8,2,1])
```


---
<!--
    må ikke bruke a, kunne skrevet det sånn
    -->
  
# Stateful Computations
## Stack

```hs
import Control.Monad.State  

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop    
```



---
<!--
    Gjøre noe litt mer komplisert.
    
    poppe -> 5 == pushe tilbake og stop
    poppe -> !5 == pushe 3 og 8 tilbake
    -->
  
# Stateful Computations
## Stack

```hs
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8  
```



---
<!--
    Begge returnerer stateful computations, derfor kan vi lime dem sammen. 
    -->
  
# Stateful Computations
## Stack

```hs
moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()  
```


---
<!--
    State monaden har to funsjoner get og put.
    
    get presenterer nåvrende tilstand som resultat
    
    put tar inn tilstand og bytter ut nåværende tilstand med ny tilstand
    -->
  
# Stateful Computations
## Getting and setting state

```hs
get = State $ \s -> (s,s)  
```

```hs
put newState = State $ \s -> ((),newState)  
```

---
<!--
    State monaden har to funsjoner get og put.
    
    get presenterer nåvrende tilstand som resultat
    
    put tar inn tilstand og bytter ut nåværende tilstand med ny tilstand
    -->
  
# Stateful Computations
## Getting and setting state

```hs
stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1] 
```

---
<!--
    Random har denne typen
    
    tar inn en generator og returnerer et tilfeldig tall sammen med en generator.
    
    
    -->
  
# Stateful Computations
## Randomness and the state monad

```hs
random :: (RandomGen g, Random a) => g -> (a, g)  
```


---
<!--
    Ser at den er en stateful computation og kan wrappe den  i state
    
    -->
  
# Stateful Computations
## Randomness and the state monad

```hs
import System.Random  
import Control.Monad.State  

randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random  
```
---
<!--
    Hvis vi vil kaste tre myner nå
    -->
  
# Stateful Computations
## Randomness and the state monad

```hs
import System.Random  
import Control.Monad.State  

threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt   
    return (a,b,c)  
```

---
<!--
    gir dette resultatet
    -->
  
# Stateful Computations
## Randomness and the state monad
```hs
import System.Random  
import Control.Monad.State  

threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt   
    c <- randomSt  
    return (a,b,c)  
```

```
ghci> runState threeCoins (mkStdGen 33)  
((True,False,True),680029187 2103410263)   
```


---
<!--
    Har brukt Maybe for å representere feil. Skal nå bruke Either
    
    Right gir riktig svar, Left gir en feil.
    -->
  
# Errors

## Either
```
ghci> :t Right 4  
Right 4 :: (Num t) => Either a t  
ghci> :t Left "out of cheese error"  
Left "out of cheese error" :: Either [Char] b  
```


---
<!--
    Er en avansert Maybe
    
    Return putter verdi i Right, som er en minimal context
    
    bind har en case for Right og Left
    i Right-caset appliserer den f på verdien x i Right.
    i Left-caset ignoreres f og Left med verdien blir bare returnert. 
    -->
  
# Errors

## Either  
```hs
instance (Error e) => Monad (Either e) where  
    return x = Right x   
    Right x >>= f = f x  
    Left err >>= f = Left err  
    fail msg = Left (strMsg msg)  
```


---
<!--
    
eksempler

Når left er feedet inn i bind, så blir funksjonen ignorert og Left verdien returnert.
Når en Right verdi blir feedet inn, så blir funksjonen applisert på verdien inne i Right Verdien.


Kunne brukt Either til å implementere balanse-eksempelet fra forrige kapittel, men enne gangen
kunne vi git med en kontekst om hvor mange fugler som var på begge sider når pierre mister balansen.
    -->
  
# Errors

## Either  
```
ghci> Left "boom" >>= \x -> return (x+1)  
Left "boom"  
ghci> Right 3 >>= \x -> return (x + 100)  
Right 103
```


---
<!--
    Tar en funksjon og en monadisk verdi, og mapper funksjonen over den monadiske verdien
    
    
-->
  
# Some Useful Monadic Functions

## liftM
```hs
liftM :: (Monad m) => (a -> b) -> m a -> m b  
```

---
<!--
    
    sammenlignbarn med fmap, bare for monadiske verdier
-->
  
# Some Useful Monadic Functions

## liftM
```hs
liftM :: (Monad m) => (a -> b) -> m a -> m b  
```

```hs
fmap :: (Functor f) => (a -> b) -> f a -> f b  
```

---
<!--
    Noen eksempler
-->
  
# Some Useful Monadic Functions

## liftM
```
ghci> liftM (*3) (Just 8)  
Just 24  
ghci> fmap (*3) (Just 8)  
Just 24  
ghci> runWriter $ liftM not $ Writer (True, "chickpeas")  
(False,"chickpeas")  
ghci> runWriter $ fmap not $ Writer (True, "chickpeas")  
(False,"chickpeas")  
ghci> runState (liftM (+100) pop) [1,2,3,4]  
(101,[2,3,4])  
ghci> runState (fmap (+100) pop) [1,2,3,4]  
(101,[2,3,4]) 
```

---

<!--
    implementasjon
    
    Implementert uten å bruke functor typeklassen.
    Kan implementere fmap eller liftM ved å bruke kun verktøyene monader gir deg
    Betyr at monader er minst like kraftige som functorer.
    
-->
  
# Some Useful Monadic Functions

## liftM


```hs 
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x))  
```

---

<!--
    Applicative functors lar oss applisere funksjoner mellom verdier med kontekst.
-->
  
# Some Useful Monadic Functions

## ap


```hs 
ghci> (+) <$> Just 3 <*> Just 5  
Just 8  
ghci> (+) <$> Just 3 <*> Nothing  
Nothing  
```
---

<!--
    Applicative functors lar oss applisere funksjoner mellom verdier med kontekst.
    
    ap kan også implementeres med kun det monader gir oss.
    
    Så monader er minst like kraftige som applikative også.
    
    Når du lager en monad kan du fort implementere en applicative ved å si at
    pure er return, og apply er ap
    
    Samme med functor, si at fmap er liftM
-->
  
# Some Useful Monadic Functions

## ap(ply)


```hs
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  
```

```hs 
ap :: (Monad m) => m (a -> b) -> m a -> m b  
ap mf m = do  
f <- mf  
x <- m  
return (f x)   
```

---


<!--
    Hvis du har en monadisk verdi som resulterer i en annen monadisk verdi.
    Kan du flattene den?
    
    gjøre om just just 9 til just 9
-->

# Some Useful Monadic Functions

## join


```hs
Just (Just 9)
```
 
```hs
join :: (Monad m) => m (m a) -> m a  
```


---


<!--
Noen eksempler med Maybe
-->

# Some Useful Monadic Functions

## join


```
ghci> join (Just (Just 9))  
Just 9  
ghci> join (Just Nothing)  
Nothing  
ghci> join Nothing  
Nothing  
```

---
<!--
    flatten på lister er intutitvt
-->
# Some Useful Monadic Functions
## join
```
ghci> join [[1,2,3],[4,5,6]]  
[1,2,3,4,5,6]  
```


---
<!--
    for writer må man bruke mappend
-->
# Some Useful Monadic Functions
## join
```
ghci> runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))  
(1,"bbbaaa") 
```

---
<!--
    implementasjon
    
    pakker ut monadisk verdi fra mm som er en moandisk verdi med en monadisk verdi
    
    Det mest interessante med join er at for hver moande, når man mater en monadisk verdi
    til en funksjon med bind, er det samme som mappe funksjnen over verdien og bruke join
    for å flate den ut.
-->
# Some Useful Monadic Functions
## join
```hs
join :: (Monad m) => m (m a) -> m a  
join mm = do  
    m <- mm  
    m  
```

---
<!--
    implementasjon
    
    pakker ut monadisk verdi fra mm som er en moandisk verdi med en monadisk verdi
    
    Det mest interessante med join er at for hver moande, når man mater en monadisk verdi
    til en funksjon med bind, er det samme som mappe funksjnen over verdien og bruke join
    for å flate den ut.
-->
# Some Useful Monadic Functions
## join

```hs
m >>= f
```

er det samme som

```hs
join (fmap f m)
```

eks:
```
ghci> fmap (\x -> Just (x+1)) (Just 9)
Just (Just 10)
```


---
<!--
    Er en filter som opererer med en predicat-funksjon som også returnerer en kontekst.
    Konteksten skal også returnerer som en del av den filtrerte listen.
    
    Resultatet av filterM er også en monadisk liste.
    
    Skal filtrere alle tall som er under 4
    starter med vanlig filter
-->
# Some Useful Monadic Functions
## filterM

```hs
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]  
```



---
<!--
    enkelt
-->
# Some Useful Monadic Functions
## filterM

```hs
ghci> filter (\x -> x < 4) [9,1,5,2,10,3]  
[1,2,3]  
```


---
<!--
    så lager vi et predicat som også logger
-->
# Some Useful Monadic Functions
## filterM

```hs
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  
```

---
<!--
    Resultatet
-->
# Some Useful Monadic Functions
## filterM

```hs
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  
```
```
ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
[1,2,3]
```

---
<!--
    Loggger-konteksten
-->
# Some Useful Monadic Functions
## filterM

```hs
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  
```

```
ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
9 is too large, throwing it away  
Keeping 1  
5 is too large, throwing it away  
Keeping 2  
10 is too large, throwing it away  
Keeping 3  
```


---
<!--
    Verdien til funksjonen returnerer er monadisk
    Derfor er også resultatet av hele funksjonen monadisk
-->
# Some Useful Monadic Functions
## foldM

```hs
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a  
```

---
<!--
    Summere en liste av tall
-->
# Some Useful Monadic Functions
## foldM

```
ghci> foldl (\acc x -> acc + x) 0 [2,8,3,1]  
14  
```

---
<!--
    Hva hvis vi vil summere en liste men dersom ett av tallene er større en 9
    skal hele summeringen feile.
    
    Lager en binærfunksjon som feiler om tallet er større enn ni, ellers fortsetter
    som den skal
-->
# Some Useful Monadic Functions
## foldM

```hs
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)  
```


---
<!--
Ser at første går bra og andre returnerer nothing.

Kan også ha en binærfunksjon som logger på samme måte som i forrige eksempel.
-->
# Some Useful Monadic Functions
## foldM

```hs
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)  
```
```
ghci> foldM binSmalls 0 [2,8,3,1]  
Just 14  
ghci> foldM binSmalls 0 [2,11,3,1]  
Nothing  
```


---

<!--
    left fish er komponering av monadiske funksjoner
-->

# Composing Monadic Functions
Left fish

```hs
ghci> let f = (+1) . (*100)  
ghci> f 4  
401  
ghci> let g = (\x -> return (x+1)) <=< (\x -> return (x*100))  
ghci> Just 4 >>= g  
Just 401  
```


---

<!--
    Lagde denne i forrige kapittel.
-->

# Composing Monadic Functions

```hs
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight  
```

---

<!--
    Kan gjøre dette mer generisk med monadisk funksjonskomposisjon.
    
    Vi lager en liste av x antall moveKnight funksjoner
    og så kjører vi en foldR hvor binærfunksjonen er leftFish og startverdien er
    return. (ville være id om man komponerte en liste av vanlige funksjoner).
-->

# Composing Monadic Functions

```hs
import Data.List  

inMany :: Int -> KnightPos -> [KnightPos]  
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)
```

---

<!--
    Hvordan  lager vi monader. Vi lager en datastruktur so mskal modellere et aspekt
    av et problem, og så når vi ser at strukturen representerer en verdi med kontekst
    og kan oppføre seg som en Monad, gjør vi den til en instans av Monad
    
    Lage en liste [3,5,9] men tallene har forskjellige sannsynligheter for å vises.
-->

# Making Monads

```hs
[3,5,9]
```
---

<!--
    Haskell har en datatype for ratios, Data.Ratio som heter Rational
-->

# Making Monads

```hs
[(3,0.5),(5,0.25),(9,0.25)]
```

---

<!--
    Kan uttrykkes på denne måten
-->

# Making Monads

```hs
[(3,1%2),(5,1%4),(9,1%4)]  
```

```
ghci> 1%4  
1 % 4  
ghci> 1%2 + 1%2  
1 % 1  
ghci> 1%3 + 5%4  
19 % 12


```
---

<!--
    Lager en ny type for datatypen vår
-->

# Making Monads

```hs
import Data.Ratio  

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show   
```

---

<!--
    Lager en instans av en functor 
-->

# Making Monads

```hs
instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs   
```

---

<!--
    Det funggerer som det skal.
    
    Det store spørsmålet. Er dette en Monad?
    Lister er moander, så dette burde være det også.
-->

# Making Monads

```hs
ghci> fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])  
Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}  
```


---

<!--
    Return må være en minimumskontekst, og verdien må være 1 fordi det gir
    ikke mening å ha 0 sannsynlighet med en tom liste.
-->

# Making Monads

```hs
instance Monad Prob where  
    return x = Prob [(x,1%1)]  
```



---

<!--
    Bind virker vanskelig. Kan gjøre nytte av at m bind f er det samme som
    join (fmap f m).
    
    Hvordan ville vi flatte ut en probabilityList av Probability lists.
    
    Kan se for oss et case med en liste med to elementer. 25 %   for at a eller b skjer
    og 75% for at c eller d skjer.
    
    For å regne ut sannsynlghetene må vi gange sammen sannsynligheten til de innerste
    elementene med de ytterste elementene.
-->

# Making Monads
![](flat.png)


---

<!--
    Dette er caset representer i haskell
    sså nå må vi finne ut hvordan vi flater ut denne. 
-->

# Making Monads
![](flat.png)

```hs
thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)  
    ]  
```

---

<!--
    lager derfor en flatten
    multAll er en funksjon som tar en tuple av en probList og en probability
    og multipliserer alle probabilities inne i probListen med denne probabilitien
    
-->

# Making Monads

```hs
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs      
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs  
```


---

<!--
    Da har vi det vi trenger for å lage monaden vår.
    Veldig enkelt siden vi allerede har gjort den vanskelige jobben.
    
    Nå må vi bare sjekke at de monadiske lovene også holder
-->

# Making Monads

```hs
instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob []  
```


---

<!--
    Hva kan vi gjøre med den?
    
    kan gjøre sannsynlighetsregning. Si vi har to normale mynter og en ladd
    som lander tails 9/10 ganger. Hvis vi kaster alle samtidig, hva er sannsynligheten
    for at alle lander på tails
    
    Lager først to vanlige mynter og en "ladd" mynt.
-->

# Making Monads

```hs
data Coin = Heads | Tails deriving (Show, Eq)  

coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  

loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)] 
```

---

<!--
    Så flipper vi de 3 myntene samtidig, og ender opp med et resultat på 9%40 like under 25%
-->

# Making Monads

```hs
import Data.List (all)  

flipThree :: Prob Bool  
flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [a,b,c])  
```

