import Control.Monad.Writer

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)
    
instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))  

finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])  


finalCountDown' :: Int -> Writer [String] ()  
finalCountDown' 0 = do  
    tell ["0"]  
finalCountDown' x = do  
    finalCountDown' (x-1)  
    tell [show x]  

test n = mapM_ putStrLn . fromDiffList . snd. runWriter $ finalCountDown n
test' n = mapM_ putStrLn  . snd. runWriter $ finalCountDown' n