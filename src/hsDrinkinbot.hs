import Data.List
import Text.Regex
import Control.Monad.State
import System.Cmd
import System.Random
import Control.Concurrent (threadDelay)

type Name  = String
type Drink = String

data Record = Record { drinker :: Name, had :: [Drink]} 

instance Show Record where
    show r = (drinker r) ++ " had " ++ (unwords $ had r)
data Bot = Bot { drinkers :: [Name], 
                 drinks :: [Drink],
                 says :: [String],
                 record :: [Record] 
                 } deriving Show

type BotState = StateT Bot IO ()

main = runBot $ do initBot "./players" "./drinks" "./says"
                   doBot

runBot f = 
    runStateT (forever $ (f >> (liftIO $ threadDelay 10000000) )) (Bot [] [] [] []) >> return () 


initBot playersFile drinksFile sayFile = 
    do players <- readConfig playersFile
       drinks  <- readConfig drinksFile
       says    <- readConfig sayFile
       updatePlayers players
       updateDrinks drinks
       updateSays says


doBot = do
    clearScreen
    printHeader
    say "Goaoaong"
    sentence <- pickSentence
    if (requiresAction sentence) then do 
        player <- pickPlayer
        drink  <- pickDrink
        updateRecord player drink
        say $ substitute sentence $ zip  ["NAME", "DRINK"] [player, drink]
    else say sentence
    say "Drink drink. Drink drink"
    printStats

printHeader = liftIO $ putStrLn header 
clearScreen = liftIO $ system "clear"

printStats = do
    b <- get
    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    liftIO $ mapM_ print $ record b 


say x = do 
    liftIO $ putStrLn x
    liftIO $ system $ "echo '" ++ x ++ "' | festival --tts" 

requiresAction sentence = (has "NAME" ) && (has "DRINK" )
    where has foo = Nothing /= matchRegex (mkRegex foo) sentence

updateRecord player drink = do
    b <- get 
    let recs = record b
    put $ b {record = (addToRecord recs player drink)}

addToRecord recs player drink = if exists player then map f recs
                                else (Record{ drinker=player, had=[drink]}):recs
    where f rec = if (drinker rec) == player then rec { had = drink:(had rec)} else rec
          exists player = elem player $ map drinker recs

pickR list = do  n <- liftIO $ randomRIO (0, (length list)-1)
                 return $ list !! n

pick f = do
    b <- get
    let s = f b
    pickR s

pickSentence = pick says 

pickPlayer = pick drinkers 

pickDrink = pick drinks 

updateDrinks d = do
    b <- get
    put $ b {drinks = d}

updateSays s = do
    b <- get
    put $ b {says = s}

updatePlayers players = do --that is bad for when players enter..
    b <- get
    put $ b {drinkers = players}


substitute sentence srList = foldr (\(search,repl) s -> substitute1 s search repl) sentence srList

substitute1 sentence search replace = subRegex (mkRegex search) sentence replace


wordMap f l = unwords $ map f $ words l 

readConfig file = do foo <- liftIO $ readFile file
                     return $ lines foo

header = unlines [  "      /|               |\\ ",
                    "     / |               | \\ ",
                    "    /  |               |  \\ ",
                    "   /   |  DRINKING BOT |   \\ ",
                    "  /    |               |    \\ ",
                    " /     |               |     \\ ",
                    "/______|               |______\\ "]
