import Data.List (all)  
import Data.Ratio  
  
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs
    
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs      
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs
    
instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)

instance Applicative Prob where
  pure x = return x
  (Prob fs) <*> (Prob xs) = Prob $ concatMap apply fs
    where apply (f, p) = map (\(x, r) -> (f x, p*r)) xs
    
data Coin = Heads | Tails deriving (Show, Eq)  
  
coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  
  
loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool  
flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [c])