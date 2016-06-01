{-# LANGUAGE Arrows, FlexibleInstances #-}

import FRP.Yampa

import Control.Monad
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar,newTVar,modifyTVar,swapTVar,readTVarIO,readTVar,writeTVar)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.List (maximumBy)
import Data.Maybe

import Network.XmlRpc.Server (fun)

-- TODO : asi předělat na Data.Aeson 
import Text.JSON
import Text.JSON.String



import XmlRpcServer
import WebServer

--- nové : WebServer pro dálkový puštění --------------------------------------------------------------

runAntServer :: IO ()
runAntServer = do
  putStrLn "Starting AntServer ..."
  runWebServer 8080 antServerFun

antServerFun :: String -> String
antServerFun cmd = case cmd of
  "santaFe"    -> encode santaFe
  "antProgram" -> "{\"antProgram\":"++ wStr ++"}"
  cmd -> "Neumim cmd: "++ cmd 




antWorldToJsonString :: AntWorld -> String
antWorldToJsonString = encode

antWorldToJson :: AntWorld -> JSValue
antWorldToJson w = makeObj [
    ("worldSize",      showJSON $ worldSize w),
    ("foodSet",        showJSON $ foodSet w),
    ("limitSteps",     showJSON $ limitSteps w),
    ("remainingSteps", showJSON $ remainingSteps w),
    ("numFoodEaten",   showJSON $ numFoodEaten w),
    ("antMode",        showJSON $ antMode w),
    ("antPos",         showJSON $ antPos w),
    ("antDir",         showJSON $ antDir w),
    ("pathSet",        showJSON $ pathSet w),
    ("frameModulo",    showJSON $ frameModulo w)
  ]


jsonToAntWorld :: JSValue -> Result AntWorld
jsonToAntWorld = undefined

foodSetToJson :: Poses -> JSValue
foodSetToJson = showJSON

instance JSON AntWorld where
  showJSON = antWorldToJson
  readJSON = jsonToAntWorld

mkShowJsonByShow :: Show a => a -> JSValue
mkShowJsonByShow = showJSON . show

mkReadJsonByRead :: Read a => JSValue -> Result a
mkReadJsonByRead jsVal = case strResult of
  Ok str -> case maybeRead str of
    Just x  -> Ok x
    Nothing -> Error "mkReadJsonByRead: Unable to read."
  Error msg -> Error $ "mkReadJsonByRead: " ++ msg 
 where
  strResult :: Result String
  strResult = readJSON jsVal


instance JSON AntMode where
  showJSON = mkShowJsonByShow
  readJSON = mkReadJsonByRead

instance JSON Dir where
  showJSON = mkShowJsonByShow
  readJSON = mkReadJsonByRead



maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

------------------------------------------------------------------------------------------------------

main = runServer

runWinner :: IO ()
runWinner = runAnt santaFe winner

winner :: AntProg
winner = antRepeat $ ifa m (pr3 l (pr2 (ifa m r) (pr2 r (pr2 l r))) (pr2 (ifa m l) m))

santaFe :: AntWorld
santaFe = mkAntWorld EatMode 600 (-3) --(-1) --(-3)

             [" . F F F . . . . . . . . . . . . . . . . . . . . . . . . . . . . "
             ," . . . F . . . . . . . . . . . . . . . . . . . . . . . . . . . . "
             ," . . . F . . . . . . . . . . . . . . . . . . . . . F F F . . . . "
             ," . . . F . . . . . . . . . . . . . . . . . . . . F . . . . F . . "
             ," . . . F . . . . . . . . . . . . . . . . . . . . F . . . . F . . "
             ," . . . F F F F . F F F F F . . . . . . . . F F . . . . . . . . . "
             ," . . . . . . . . . . . . F . . . . . . . . . . . . . . . . F . . "
             ," . . . . . . . . . . . . F . . . . . . . F . . . . . . . . . . . "
             ," . . . . . . . . . . . . F . . . . . . . F . . . . . . . . . . . "
             ," . . . . . . . . . . . . F . . . . . . . F . . . . . . . . F . . "
             ," . . . . . . . . . . . . . . . . . . . . F . . . . . . . . . . . "
             ," . . . . . . . . . . . . F . . . . . . . . . . . . . . . . . . . "
             ," . . . . . . . . . . . . F . . . . . . . . . . . . . . . . F . . "
             ," . . . . . . . . . . . . F . . . . . . . F . . . . . . . . . . . "
             ," . . . . . . . . . . . . F . . . . . . . F . . . . . F F F . . . "
             ," . . . . . . . . . . . . . . . . . F . . . . . F . . . . . . . . "
             ," . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . "
             ," . . . . . . . . . . . . F . . . . . . . . . . . . . . . . . . . "            
             ," . . . . . . . . . . . . F . . . F . . . . . . . F . . . . . . . "
             ," . . . . . . . . . . . . F . . . F . . . . . . . . . . F . . . . "
             ," . . . . . . . . . . . . F . . . F . . . . . . . . . . . . . . . "            
             ," . . . . . . . . . . . . F . . . F . . . . . . . . . . . . . . . "
             ," . . . . . . . . . . . . F . . . . . . . . . . . . . F . . . . . "
             ," . . . . . . . . . . . . F . . . . . . . . . . F . . . . . . . . "
             ," . . . F F . . F F F F F . . . . F . . . . . . . . . . . . . . . "
             ," . F . . . . . . . . . . . . . . F . . . . . . . . . . . . . . . "
             ," . F . . . . . . . . . . . . . . F . . . . . . . . . . . . . . . "
             ," . F . . . . . . F F F F F F F . . . . . . . . . . . . . . . . . "
             ," . F . . . . . F . . . . . . . . . . . . . . . . . . . . . . . . "
             ," . . . . . . . F . . . . . . . . . . . . . . . . . . . . . . . . "
             ," . . F F F F . . . . . . . . . . . . . . . . . . . . . . . . . . "
             ," . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ."]

emptyWorld :: AntWorld
emptyWorld = mkEmptyWorld EatMode 600 (-4) (Just (32,32))



emptyWorld_b :: AntWorld
emptyWorld_b = toggleAntMode emptyWorld

wMut :: AntProg
wMut = antRepeat $ ifa (pr2 r m) (pr3 (pr2 d l) (pr2 (ifa m r) (pr2 r (pr2 l r))) (pr2 (ifa m l) m))


kozaEater :: AntTree
kozaEater = IFA M (P3 L (P2 (IFA M R) (P2 R (P2 L R))) (P2 (IFA M L) M))


spiralBuilder :: AntTree
spiralBuilder = IFA L (P2 M D)


---------------------------------------------------------------------------------------------------------

runAnt :: AntWorld -> AntProg -> IO ()
runAnt antWorld antProg = do
    antWorld_TV <- atomically $ newTVar antWorld
    reactimate 
      (antInitialize antWorld)     
      (antInputFun   antWorld_TV)
      (antOutputFun  antWorld_TV)
      (time &&& antProg)

antInitialize :: AntWorld -> IO AntWorld
antInitialize w = do 
  if frameModulo w >= 0 then putWorld w else return () 
  return w

antInputFun :: TVar AntWorld -> Bool -> IO (DTime, Maybe AntWorld)
antInputFun tvWorld _ = do
  currentWorld <- readTVarIO tvWorld
  return (antStep, Just currentWorld)


antOutputFun :: TVar AntWorld -> Bool -> (Time, Event AntCmd)-> IO Bool
antOutputFun tvWorld _ (t, antCmdEvent) = do
  
  outOfSteps <- case antCmdEvent of
    Event (AntAct action) -> do
      atomically $ modifyTVar tvWorld (doAntAction action)
      w <- readTVarIO tvWorld 
      
      let step = (round :: Time -> Int) t
      let isLimit = remainingSteps w <= 0
      let q = frameModulo w

      if ((q > 0) && ((step `mod` q) == 0)) || (isLimit && q >= -1) then do
        putStrLn ("t=" ++ show t ++ ": " ++ show antCmdEvent)
        putWorld w 
      else
        return () 
      
      return isLimit
    _ -> return False

  return $ (isEnd antCmdEvent) || outOfSteps


---------------------------------------------------------------------------------------------------

type Score = Int
data AntTree = L | R | M | D | IFA AntTree AntTree | P2 AntTree AntTree | P3 AntTree AntTree AntTree deriving (Eq,Show)


evalAnt :: AntWorld -> AntTree -> IO (Score,AntWorld)
evalAnt antWorld antTree = do
  tvWorld <- atomically $ newTVar antWorld
  reactimate 
    (antInitialize antWorld)     
    (antInputFun   tvWorld)
    (antOutputFun  tvWorld)
    (time &&& (fromAntTree antTree))
  w <- readTVarIO tvWorld
  let score = numFoodEaten w
  putEvalInfo w antTree score
  return (score,w)


putEvalInfo :: AntWorld -> AntTree -> Score -> IO ()
putEvalInfo w antTree score =     
  if frameModulo w >= -2 
    then (putStrLn $ (show score) ++"\t"++ (show antTree)) 
    else if frameModulo w >= -3 
      then (putStr $ (show score)++", ") 
      else return ()



evalBuildAntByEatAnt :: AntTree -> AntWorld -> AntTree -> IO (Score,AntWorld)
evalBuildAntByEatAnt eater w builder = do
  (buildScore, wNew) <- evalBuildAnt w builder
  (eatScore,   wEnd) <- evalEatAnt (resetAntWorld EatMode wNew) eater
  return (buildScore - eatScore, wEnd)  




antGame_beta :: AntWorld -> AntTree -> AntTree -> IO (Score,Score,AntWorld)
antGame_beta w ant1 ant2 = undefined



evalEatAnt :: AntWorld -> AntTree -> IO (Score,AntWorld)
evalEatAnt antWorld antTree = 
  evalAnt (setAntMode antWorld EatMode) antTree


evalBuildAnt :: AntWorld -> AntTree -> IO (Score,AntWorld)
evalBuildAnt antWorld antTree = do
  (score,w) <- evalAnt (setAntMode antWorld BuildMode) antTree
  return (-score,w)



auxFst :: (AntTree -> IO (Score,AntWorld)) -> (AntTree -> IO Score)
auxFst f tree = do
  (s,_) <- f tree
  return s 


evalAnts' :: AntWorld -> [AntTree] -> IO [Score]
evalAnts' = mapM . auxFst . evalAnt



evalAnts :: AntWorld -> [AntTree] -> IO [Score]
evalAnts = genericEvalAnts evalAnt


genericEvalAnts :: (AntWorld -> AntTree -> IO (Score,AntWorld)) -> (AntWorld -> [AntTree] -> IO [Score])
genericEvalAnts antEvaluator w trees = do
  scores <- (mapM . auxFst . antEvaluator) w trees
  let (iBest,bestScore) = maximumBy (\(_,x)(_,y)->compare x y) $ zip [0..] scores
  let bestTree = trees !! iBest
  (_,wBestEnd) <- antEvaluator w bestTree -- TODO !!!!
  putWorld wBestEnd 
  return scores


evalEatAnts :: AntWorld -> [AntTree] -> IO [Score]
evalEatAnts = genericEvalAnts evalEatAnt

evalBuildAnts :: AntWorld -> [AntTree] -> IO [Score]
evalBuildAnts = genericEvalAnts evalBuildAnt

evalBuildAntsByEatAnt :: AntTree -> AntWorld -> [AntTree] -> IO [Score]
evalBuildAntsByEatAnt eater = genericEvalAnts (evalBuildAntByEatAnt eater) 



putEvalAnt :: AntWorld -> AntTree -> IO ()
putEvalAnt w t = putWorld . snd =<< evalAnt w t

---------------------------------------------------------------------------------------------------

data AntWorld = AntWorld {
  worldSize :: Maybe Size,
  foodSet :: Poses,
  limitSteps :: Int,
  remainingSteps :: Int,
  numFoodEaten :: Int,
  antMode :: AntMode,
  antPos :: Pos,
  antDir :: Dir,
  pathSet :: Poses,
  frameModulo :: Int
} deriving (Show)

type Size  = (Int,Int) -- (w,h) 
type Pos   = (Int,Int) -- (x,y)
type Poses = Set Pos

data Dir     = DUp | DDown | DLeft | DRight deriving (Show,Read,Eq)
data AntMode = EatMode | BuildMode deriving (Show,Read,Eq)


mkEmptyWorld :: AntMode -> Int -> Int -> Maybe Size -> AntWorld
mkEmptyWorld initAntMode limit frameMod size = AntWorld {
   worldSize = size,
   foodSet = Set.empty,
   limitSteps = limit,
   remainingSteps = limit,
   numFoodEaten = 0,
   antMode = initAntMode,
   antPos = (0,0),  -- TODO
   antDir = DRight, -- TODO
   pathSet = Set.empty,
   frameModulo = frameMod
}

-- todo : neni size obrácenì ???

mkAntWorld :: AntMode -> Int -> Int -> [[Char]] -> AntWorld
mkAntWorld initAntMode limit frameMod wStrs' = 
  let wStrs = map (filter (/=' ')) wStrs'
      size = Just (length wStrs, maximum $ map length wStrs)
      addFood (ch,pos) acc = if ch == 'F' then Set.insert pos acc else acc
      chPoses = concat $ map (\(row,i)-> zip row (map (\j->(j,i)) [0..])) $ zip wStrs [0..]
      fSet = foldr addFood Set.empty chPoses
      w = mkEmptyWorld initAntMode limit frameMod size
   in w { foodSet = fSet  }      



resetAntWorld :: AntMode -> AntWorld -> AntWorld
resetAntWorld newMode w = w {
   remainingSteps = limitSteps w,
   numFoodEaten = 0,
   antMode = newMode,
   antPos = (0,0),  -- TODO
   antDir = DRight, -- TODO
   pathSet = Set.empty   
}


{-
mkAntWorld_old :: AntMode -> Int -> Int -> [[Char]] -> AntWorld
mkAntWorld_old initAntMode limit frameMod wStrs' = AntWorld {
   antPos = (0,0),  -- TODO
   antDir = DRight, -- TODO
   worldSize = (length wStrs, maximum $ map length wStrs),
   foodSet = fSet,
   pathSet = Set.empty,
   remainingSteps = limit,
   numFoodEaten = 0,
   frameModulo = frameMod,
   antMode = initAntMode
} where 
   wStrs = map (filter (/=' ')) wStrs'
   addFood (ch,pos) acc = if ch == 'F' then Set.insert pos acc else acc
   fSet = foldr addFood Set.empty chPoses
   chPoses = concat $ map (\(row,i)-> zip row (map (\j->(j,i)) [0..])) $ zip wStrs [0..]
-}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

putWorld w = putStrLn $ "\n" ++ 
  asciiWorld w ++
  " E A T E N  FOOD : " ++ (show $ numFoodEaten w) ++"\n" ++ 
  " Is  Food  Ahead : " ++ (show $ isFoodAhead w) ++"\n" ++ 
  " remaining steps : " ++ (show $ remainingSteps w) ++"\n"

putWorldDump w = putStrLn $ "\n" ++ show w ++ "\n\n" ++ asciiWorld w


asciiWorld :: AntWorld -> String
asciiWorld w = concatMap drawPixel poses
  where
    aPos = antPos w
    aDir = antDir w
    pSet = pathSet w
    fSet = foodSet w
    Just (xSize, ySize) = worldSize w
    (xMax,  yMax)  = (xSize-1, ySize-1)
    poses = [(x,y) | y <- [0..yMax], x <- [0..xMax]]
    drawPixel pos@(x,_) = 
      let pixel | pos == aPos = showAnt aDir
                | Set.member pos fSet = 'F'
                | Set.member pos pSet = '@'
                | otherwise           = '.'
          pixelStr = pixel : " "
       in if x == xMax 
          then pixelStr ++ "\n" 
          else pixelStr


showAnt :: Dir -> Char
showAnt dir = case dir of 
  DUp    -> 'A'
  DDown  -> 'V'
  DLeft  -> '<'
  DRight -> '>' 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

forwardPos :: AntWorld -> Pos 
forwardPos w = case worldSize w of
  Just size -> changePosToroid size (antDir w) (antPos w)
  Nothing   -> changePosUnlimited   (antDir w) (antPos w)
 where
  changePosToroid :: Size -> Dir -> Pos -> Pos
  changePosToroid (xSize,ySize) dir (x,y) = case dir of
    DLeft  -> if x == 0 then (xSize-1, y) else (x-1, y)
    DUp    -> if y == 0 then (x, ySize-1) else (x ,y-1)
    DRight -> if x == xSize-1 then (0, y) else (x+1, y)
    DDown  -> if y == ySize-1 then (x, 0) else (x, y+1)

  changePosUnlimited :: Dir -> Pos -> Pos
  changePosUnlimited dir (x,y) = case dir of
    DLeft  -> (x-1, y)
    DUp    -> (x ,y-1)
    DRight -> (x+1, y)
    DDown  -> (x, y+1)

   

isFoodAhead :: AntWorld -> Bool
isFoodAhead w = Set.member (forwardPos w) (foodSet w)

isEatMode :: AntWorld -> Bool
isEatMode w = EatMode == antMode w

-----------------------------------------------------------------------------------------


data AntAction = AntLeft | AntRight | AntMove | AntDrop deriving (Eq)

instance Show AntAction where
  show a = case a of
    AntLeft  -> "left"
    AntRight -> "right"
    AntMove  -> "move"
    AntDrop  -> "drop"

setAntMode :: AntWorld -> AntMode -> AntWorld
setAntMode w newMode = w { antMode = newMode }

toggleAntMode :: AntWorld -> AntWorld
toggleAntMode w = w { antMode = toggle $ antMode w }
  where 
    toggle :: AntMode -> AntMode
    toggle EatMode   = BuildMode
    toggle BuildMode = EatMode

doAntAction :: AntAction -> AntWorld -> AntWorld
doAntAction a w =
 let w' = w { remainingSteps = (remainingSteps w) - 1 }
  in case a of
   AntLeft  -> w' { antDir  = turnLeft  $ antDir w }
   AntRight -> w' { antDir  = turnRight $ antDir w }
   AntMove  -> auxMoveForward w'
   AntDrop  -> auxDropFood w' -- nakonec jsem zpoplatnil i drop, aby nebyl unhaltující program a vůbec je to akce

turnLeft :: Dir -> Dir
turnLeft dir = case dir of
  DUp    -> DLeft  ; DLeft  -> DDown
  DDown  -> DRight ; DRight -> DUp

turnRight :: Dir -> Dir
turnRight dir = case dir of
  DUp    -> DRight ; DRight -> DDown
  DDown  -> DLeft  ; DLeft  -> DUp

auxDropFood :: AntWorld -> AntWorld
auxDropFood w = if isEatMode w || isOnFood 
                then w 
                else w { foodSet = newFoodSet, 
                         numFoodEaten = (numFoodEaten w)-1 }
  where 
    aPos = antPos w
    fSet = foodSet w
    isOnFood = Set.member aPos fSet
    newFoodSet = Set.insert aPos fSet

auxMoveForward :: AntWorld -> AntWorld
auxMoveForward w = 
  let newAntPos = forwardPos w
      fSet = foodSet w
      isOnFood = Set.member newAntPos fSet
      numEaten = numFoodEaten w
   in w  { pathSet      = Set.insert (antPos w) (pathSet w),
           antPos       = newAntPos,
           numFoodEaten = if isOnFood then 1+numEaten else numEaten,
           foodSet      = if isOnFood then Set.delete newAntPos fSet else fSet }



doAntActions :: AntWorld -> [AntAction] -> AntWorld
doAntActions = foldl (flip doAntAction)


----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

type AntProg = SF AntWorld (Event AntCmd)

antStep :: DTime
antStep = 1

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  

data AntCmd  = AntAct AntAction | AntEnd deriving (Eq)

class HasEnd a where
  isEnd :: a -> Bool

endAlarm :: HasEnd a => SF a (Event ())
endAlarm = (arr isEnd) >>> edge


instance HasEnd AntCmd where
  isEnd AntEnd = True
  isEnd _      = False

instance Show AntCmd where
  show cmd = case cmd of
    AntEnd   -> "HALT!"
    AntAct a -> show a


instance HasEnd a => HasEnd (Event a) where
  isEnd e = event False isEnd e

showEvent :: Show a => Event a -> String
showEvent NoEvent   = "NO-EVENT"
showEvent (Event x) = show x


---------------------------------------------------------

mkActionProg :: AntAction -> AntProg
mkActionProg action = fireTwice (AntAct action) antStep AntEnd

antLeft, antRight, antMove, antDrop :: AntProg
antLeft  = mkActionProg AntLeft
antRight = mkActionProg AntRight
antMove  = mkActionProg AntMove
antDrop  = mkActionProg AntDrop

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

fireTwice :: b -> DTime -> b -> SF a (Event b)
fireTwice x dt y = dtAux (now x) dt (now y)

dtAux :: SF a (Event b) -> DTime -> SF a (Event b) -> SF a (Event b)
dtAux sf1 dt sf2 = (sf1 &&& after dt ()) `switch` (\()->sf2)

fireMany :: DTime -> [b] -> SF a (Event b)
fireMany dt = foldr (\x acc -> dtAux (now x) dt acc) never 

----------------------------------------------------------------------------

antRepeat :: AntProg -> AntProg
antRepeat p = do_1 p ..> repeat_2 p

antCompose :: AntProg -> AntProg -> AntProg
antCompose p1 p2 = do_1 p1 ..> do_2 p2

ifa :: AntProg -> AntProg -> AntProg
ifa p1 p2 = if_1 isFoodAhead ..> thenElse_2 p1 p2

-- --  --  --  --  --  --  --  -  -  -  -  -


(..>) :: Sw1 a -> Sw2 a -> AntProg
(..>) = switch


type Sw1 a =      SF AntWorld (Event AntCmd, Event a)
type Sw2 a = a -> SF AntWorld (Event AntCmd)



do_1 :: AntProg -> Sw1 ()
do_1 p = p >>> (identity &&& endAlarm)

if_1 :: (AntWorld -> Bool) -> Sw1 Bool
if_1 worldPred = (arr (\ w -> (NoEvent, Event $ worldPred w)))


thenElse_2 :: AntProg -> AntProg -> Sw2 Bool
thenElse_2 p1 p2 b = if b then p1 else p2

do_2 :: AntProg -> Sw2 ()
do_2 p () = p

repeat_2 :: AntProg -> Sw2 ()
repeat_2 p () = antRepeat p


--- Další kompatibilní: ...

ifa1 :: AntProg -> AntProg
ifa1 p = if_1 isFoodAhead ..> then_2 p


then_2 :: AntProg -> Sw2 Bool
then_2 p b = if b then p else (now AntEnd)


whileDo :: (AntWorld -> Bool) -> AntProg ->AntProg
whileDo wPred p = undefined






--------------------------------------------------------------------------------
-- ARCHIVe ---------------------------------------------------------------------
--------------------------------------------------------------------------------


antRepeat_old :: AntProg -> AntProg
antRepeat_old p = (p >>> (identity &&& endAlarm)) `switch` (const $ antRepeat_old p)

antCompose_old :: AntProg -> AntProg -> AntProg
antCompose_old p1 p2 = (p1 >>> (identity &&& endAlarm)) `switch` (const p2)

ifa_old :: AntProg -> AntProg -> AntProg
ifa_old p1 p2 = (arr (\ w -> (NoEvent, Event $ isFoodAhead w))) `switch` (\ isAhead -> if isAhead then p1 else p2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 


-- chybnej
ifa_old2 :: AntProg -> AntProg -> AntProg
ifa_old2 p1 p2 = proc w -> do 
  a1 <- p1 -< w
  a2 <- p2 -< w
  returnA -< if isFoodAhead w then a1 else a2

-- chybnej
ifa_old1 :: AntProg -> AntProg -> AntProg
ifa_old1 p1 p2 = (identity &&& p1 &&& p2) >>> arr (\(w,(a1,a2)) -> if isFoodAhead w then a1 else a2)



--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  





---------------------------------------------------------------------------
-- TESTs  -----------------------------------------------------------------
---------------------------------------------------------------------------


testFireTwice = runSF 0.5 () $ fireTwice "foo" 10.0 "end"

testFireMany = runSF 1.0 () $ fireMany 1.0 msgs 
  where msgs = ["foo_" ++ (show x) | x <- [1..10]] ++ ["bar", "end"]

instance HasEnd String where
  isEnd str = str == "end"


--
--
--



-------------------------------------------------------------------------------------------------------------------------------
-- HELLO WORLDs ---------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------

{-
reactimate :: IO a                         -- init    ...  první vjem a
     -> (Bool -> IO (DTime, Maybe a))      -- input   ...  další vjem a              ...   _ -> IO (čas od minulého vjemu, Maybe Vjem)
     -> (Bool -> b -> IO Bool)             -- output  ...  zpracuje akci b           ...   _ akce -> IO (Halt?)
     -> SF a b                             -- process ...  Signal Function "Agenta"  ...   "Signál Vjemů -> Signál Akcí"
     -> IO ()
-}

helloWorld = reactimate 
  (return "Hello World!") 
  (\_-> return (0.0,Nothing)) 
  (\_ x-> putStrLn x >> return True) 
  identity


helloWorld2 :: IO ()
helloWorld2 = reactimate initialize input output process
  where
    initialize :: IO Int
    initialize  = return 5

    input :: Bool -> IO (DTime, Maybe Int)
    input _ = return (0.0, Nothing)

    output :: Bool -> String -> IO Bool
    output _ x  = putStrLn x >> return True

    process :: SF Int String 
    process = arr sayNum
      where sayNum i = take i $ repeat 'i'



helloWorld3 = reactimate 
  (return ())
  (\_-> return (2,Just ())) 
  (\_ x-> putStrLn (show x) >> return (x >= 10)) 
  time

runSF :: (Show b, HasEnd b) => DTime -> a -> SF a (Event b) -> IO ()
runSF stepDelay constInput sf = reactimate 
    (return constInput)
    (const . return $ (stepDelay, Just constInput)) 
    outputFun
    (sf &&& time)
  where
    outputFun _ (e,t) = do
        putStrLn ("t=" ++ show t ++ ": " ++ show e)
        return $ isEnd e

{-
initAntWorld :: AntWorld
initAntWorld = AntWorld {
   antPos = (0,0),
   antDir = DRight,
   worldSize = (3,3), -- TODO
   foodSet = Set.singleton (1,2),
   pathSet = Set.empty,
   remainingSteps = 10,
   numFoodEaten = 0,
   frameModulo = 1,
   antMode = EatMode
}
-}

{-
testAntProg :: AntProg -> IO ()
testAntProg antProg = runSF antStep initAntWorld antProg
-}

---------------------------------------------

---------------------------------------------



-- XmlRpc Server....................................


runServer :: IO ()
runServer = runXmlRpcServer 4242 [
  ("evalAnt" ,     fun $ evalAntJsonStr   santaFe),
  ("evalAnts",     fun $ evalAntsJsonStr  santaFe),
  ("evalAnts'",    fun $ evalAntsJsonStr' santaFe),
  ("perfectScore", fun $ getPerfectScore  santaFe),
  -- experimental follows:
  ("evalBuildAnts",  fun $ evalBuildAntsJsonStr emptyWorld_b),
  ("evalBuildAntsByEatAnt", fun $ evalBuildAntsByEatAnt_jsonStr kozaEater emptyWorld_b)]


getPerfectScore :: AntWorld -> String -> IO Score
getPerfectScore = const . return . Set.size . foodSet  

evalAntJsonStr :: AntWorld -> String -> IO Score
evalAntJsonStr w = return . fst <=< evalAnt w . jsonStr2antTree 
 
evalAntsJsonStr :: AntWorld -> String -> IO [Score]
evalAntsJsonStr w = evalAnts w . jsonStr2antTrees

evalBuildAntsJsonStr :: AntWorld -> String -> IO [Score]
evalBuildAntsJsonStr w = evalBuildAnts w . jsonStr2antTrees

evalBuildAntsByEatAnt_jsonStr :: AntTree -> AntWorld -> String -> IO [Score]
evalBuildAntsByEatAnt_jsonStr eater w = evalBuildAntsByEatAnt eater w . jsonStr2antTrees


evalAntsJsonStr' :: AntWorld -> String -> IO [Score]
evalAntsJsonStr' w = evalAnts' w . jsonStr2antTrees



-- json stuff ---------------------

testJsonWinTree = (runAnt santaFe) . fromAntTree . jsonStr2antTree $ wStr




wStr = "[\"ifa\", \"m\", [\"pr3\", \"l\", [\"pr2\", [\"ifa\", \"m\", \"r\"], [\"pr2\", \"r\", [\"pr2\", \"l\", \"r\"]]], [\"pr2\", [\"ifa\", \"m\", \"l\"], \"m\"]]]"

jsonStr2antTrees :: String -> [AntTree]
jsonStr2antTrees str = case readJSArr str of
  Left  err   -> error err
  Right jsVal -> json2antTrees jsVal

jsonStr2antTree :: String -> AntTree
jsonStr2antTree str = case readJSArr str of
  Left  err   -> error err
  Right jsVal -> json2antTree jsVal 


--parseJSArrStr :: (JSValue -> a) -> 

readJSArr :: String -> Either String JSValue
readJSArr = runGetJSON readJSArray



json2antTrees :: JSValue -> [AntTree]
json2antTrees jsVal = case jsVal of
  JSArray xs -> map json2antTree xs

json2antTree :: JSValue -> AntTree
json2antTree jsVal = case jsVal of
  JSString x -> case fromJSString x of
    "l" -> L
    "m" -> M
    "r" -> R
    "d" -> D
  JSArray ((JSString x):xs) -> case fromJSString x of
    "ifa" -> IFA (json2antTree $ xs!!0) (json2antTree $ xs!!1)
    "pr2" -> P2  (json2antTree $ xs!!0) (json2antTree $ xs!!1)
    "pr3" -> P3  (json2antTree $ xs!!0) (json2antTree $ xs!!1) (json2antTree $ xs!!2)
  _ -> error "Unsupported JSON form.."





---------------------------------------------

testWinTree = runAnt santaFe $ fromAntTree winnerTree


winnerTree = IFA M (P3 L (P2 (IFA M R) (P2 R (P2 L R) ) ) (P2 (IFA M L) M) )


pr2 = antCompose
pr3 a b c = antCompose a (antCompose b c) 

m = antMove
l = antLeft
r = antRight
d = antDrop


fromAntTree :: AntTree -> AntProg
fromAntTree antTree = antRepeat $ f antTree
 where
  f antTree = case antTree of
    L -> antLeft
    R -> antRight
    M -> antMove
    D -> antDrop
    IFA a b -> ifa (f a) (f b)
    P2 a b -> antCompose (f a) (f b)
    P3 a b c -> antCompose (f a) $ antCompose (f b) (f c)



-- ifa m (pr3 l m r)

  --  ifa m (pr3 l m r)