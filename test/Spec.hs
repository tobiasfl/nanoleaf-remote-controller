
import EffectsTests (runTests)

main :: IO ()
main = do 
    unitTests

unitTests :: IO ()
unitTests = do
    runTests
    
