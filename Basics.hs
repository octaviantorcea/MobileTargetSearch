{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    hunter     :: Position,
    targets    :: [Target],
    obstacoles :: [Position],
    gateways   :: [(Position, Position)],
    nrLines      :: Int,
    nrColumns    :: Int
} deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString game = intercalate "\n" $ mySplit (nrColumns game) [sym i j | i <- [0..nrLines game - 1], j <- [0..nrColumns game - 1]]
    where
        sym i j
            | (i, j) == hunter game = '!'
            | (i, j) `elem` map position (targets game) = '*'
            | (i, j) `elem` map fst (gateways game) ++ map snd (gateways game) = '#'
            | (i, j) `elem` obstacoles game = '@'
            | otherwise = ' '

mySplit :: Int -> [a] -> [[a]]
mySplit _ [] = []
mySplit n list = first : mySplit n rest
    where
        (first, rest) = splitAt n list

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame l c = Game {
    nrLines = l,
    nrColumns = c,
    hunter = (1, 1),
    targets = [],
    gateways = [],
    obstacoles = [(0, i) | i <- [0..c - 1]] ++ [(i, c - 1) | i <- [1..l - 1]] ++ [(l - 1, i) | i <- [0..c - 2]] ++ [(i, 0) | i <- [1..l - 2]]
}


{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (x, y) game
    | x >= nrLines game || y >= nrColumns game || x < 0 || y < 0 = game
    | (x, y) `elem` obstacoles game = game
    | otherwise = Game {hunter = (x, y),
                        targets = targets game,
                        obstacoles = obstacoles game,
                        gateways= gateways game,
                        nrLines= nrLines game,
                        nrColumns= nrColumns game}

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behav pos game = Game {hunter = hunter game,
                                 targets = targets game ++ [Target {position = pos, behavior = behav}],
                                 obstacoles = obstacoles game,
                                 gateways= gateways game,
                                 nrLines= nrLines game,
                                 nrColumns= nrColumns game}

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (p1, p2) game = Game {hunter = hunter game,
                                 targets = targets game,
                                 obstacoles = obstacoles game,
                                 gateways= gateways game ++ [(p1, p2)],
                                 nrLines= nrLines game,
                                 nrColumns= nrColumns game}

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game = Game {hunter = hunter game,
                                 targets = targets game,
                                 obstacoles = obstacoles game ++ [pos],
                                 gateways= gateways game,
                                 nrLines= nrLines game,
                                 nrColumns= nrColumns game}

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove (x, y) game
    | (x, y) `elem` obstacoles game = Nothing
    | (x, y) `elem` map fst (gateways game) = lookup (x, y) (gateways game)
    | (x, y) `elem` map snd (gateways game) = lookup (x, y) (map (\(i, j) -> (j, i)) (gateways game))
    | otherwise = Just (x, y)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
goEast :: Behavior
goEast initPos@(iX, iY) game
    | isNothing $ attemptMove (iX, iY + 1) game = Target {position = fromJust $ attemptMove initPos game, behavior = goEast}
    | otherwise = Target {position = fromJust $ attemptMove (iX, iY + 1) game, behavior = goEast}

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest initPos@(iX, iY) game
    | isNothing $ attemptMove (iX, iY - 1) game = Target {position = fromJust $ attemptMove initPos game, behavior = goWest}
    | otherwise = Target {position = fromJust $ attemptMove (iX, iY - 1) game, behavior = goWest}

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth initPos@(iX, iY) game
    | isNothing $ attemptMove (iX - 1, iY) game = Target {position = fromJust $ attemptMove initPos game, behavior = goNorth}
    | otherwise = Target {position = fromJust $ attemptMove (iX - 1, iY) game, behavior = goNorth}

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth initPos@(iX, iY) game
    | isNothing $ attemptMove (iX + 1, iY) game = Target {position = fromJust $ attemptMove initPos game, behavior = goSouth}
    | otherwise = Target {position = fromJust $ attemptMove (iX + 1, iY) game, behavior = goSouth}

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

bounce :: Int -> Behavior
bounce a (iX, iY) game
    | isNothing $ attemptMove (iX + a, iY) game = Target {position = fromJust $ attemptMove (iX - a, iY) game, behavior = bounce (negate a)}
    | otherwise = Target {position = fromJust $ attemptMove (iX + a, iY) game, behavior = bounce a}

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
-}
moveTargets :: Game -> Game
moveTargets game = Game {hunter = hunter game,
                         targets = map (\t -> behavior t (position t) game) (targets game),
                         obstacoles = obstacoles game,
                         gateways = gateways game,
                         nrLines = nrLines game,
                         nrColumns= nrColumns game}

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x, y) t
    | (x + 1, y) == position t = True
    | (x - 1, y) == position t = True
    | (x, y + 1) == position t = True
    | (x, y - 1) == position t = True
    | otherwise = False

{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
killTargets :: Game -> Game
killTargets (Game hun@(xH, yH) t o g nL nC) = Game hun (filter (not . isTargetKilled (xH, yH)) t) o g nL nC

moveHunter' :: Position -> Game -> Position
moveHunter' pos game
    | isNothing $ attemptMove pos game = hunter game
    | otherwise = fromJust $ attemptMove pos game

moveHunter :: Direction -> Game -> Game
moveHunter dir game@(Game (x, y) t o g nL nC)
    | dir == North = Game (moveHunter' (x - 1, y) game) t o g nL nC
    | dir == East = Game (moveHunter' (x, y + 1) game) t o g nL nC
    | dir == West = Game (moveHunter' (x, y - 1) game) t o g nL nC
    | otherwise = Game (moveHunter' (x + 1, y) game) t o g nL nC

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir real game
    | real = killTargets $ moveTargets $ killTargets $ moveHunter dir game
    | otherwise = moveHunter dir game
{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = not $ null $ targets game

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(North, advanceGameState North False game),
                       (South, advanceGameState South False game),
                       (East, advanceGameState East False game),
                       (West, advanceGameState West False game)]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (Game hun t _ _ _ _) = any (isTargetKilled hun) t 

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h (Game hun t _ _ _ _) = minimum (foldl (\acc posT -> hEuclidean hun posT : acc) [] (map position t))

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
