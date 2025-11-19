{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- GameTypes.hs
-- Definición de todos los tipos de datos para Etherside

module GameTypes where

import Control.Monad.State
import System.Random
import qualified Data.Map as M
import qualified Data.Set as S

-- | Posición en el mapa (x, y)
type Position = (Int, Int)

-- | Forma del jugador
data Form = Earthly | Ghostly deriving (Eq, Show)

-- | Tipos de items
data Item = HealthPotion Int
          | EnergyPotion Int
          | AttackBoost Int
          deriving (Eq, Show)

-- | Enemigo
data Enemy = Enemy
    { ePosition :: Position
    , eHealth :: Int
    , eAttack :: Int
    , eType :: EnemyType
    } deriving (Eq, Show)

data EnemyType = Walker | Drainer deriving (Eq, Show)

-- | Jugador
data Player = Player
    { pPosition :: Position
    , pHealth :: Int
    , pMaxHealth :: Int
    , pEnergy :: Int
    , pMaxEnergy :: Int
    , pAttack :: Int
    , pForm :: Form
    , pFormCooldown :: Int
    } deriving (Eq, Show)

-- | Estado global del juego
data GameState = GameState
    { gsPlayer :: Player
    , gsEnemies :: [Enemy]
    , gsItems :: M.Map Position Item
    , gsRevealedMap :: S.Set Position
    , gsGameOver :: Bool
    , gsVictory :: Bool
    , gsMessage :: String
    , gsTurn :: Int
    , gsRng :: StdGen
    } deriving (Show)

-- | Monada State para el juego
type GameM a = State GameState a

-- | Inicializar el estado del juego
initGameState :: StdGen -> GameState
initGameState gen = GameState
    { gsPlayer = initPlayer
    , gsEnemies = initEnemies gen
    , gsItems = initItems gen
    , gsRevealedMap = S.singleton (10, 10)
    , gsGameOver = False
    , gsVictory = False
    , gsMessage = "¡Encuentra el Núcleo del Espíritu en (0,0)!"
    , gsTurn = 0
    , gsRng = gen
    }

-- | Reiniciar el juego conservando el mapa revelado
resetGameState :: GameState -> StdGen -> GameState
resetGameState oldGs gen = GameState
    { gsPlayer = initPlayer
    , gsEnemies = initEnemies gen
    , gsItems = initItems gen
    , gsRevealedMap = gsRevealedMap oldGs  -- Mantener mapa revelado
    , gsGameOver = False
    , gsVictory = False
    , gsMessage = "¡Reintentando! El mapa revelado se conserva."
    , gsTurn = 0
    , gsRng = gen
    }

-- | Inicializar jugador
initPlayer :: Player
initPlayer = Player
    { pPosition = (10, 10)
    , pHealth = 100
    , pMaxHealth = 100
    , pEnergy = 100
    , pMaxEnergy = 100
    , pAttack = 10
    , pForm = Earthly
    , pFormCooldown = 0
    }

-- | Generar enemigos iniciales
initEnemies :: StdGen -> [Enemy]
initEnemies gen = take 8 $ zipWith3 makeEnemy positions healths types
  where
    (gen1, gen2) = splitGen gen
    positions = take 8 $ zip (randomRs (5, 15) gen1) (randomRs (5, 15) gen2)
    healths = randomRs (20, 40) gen1
    types = cycle [Walker, Drainer]
    makeEnemy pos hp t = Enemy pos hp 5 t

-- | Generar items iniciales
initItems :: StdGen -> M.Map Position Item
initItems gen = M.fromList $ take 10 itemList
  where
    (gen1, gen2) = splitGen gen
    positions = zip (randomRs (3, 20) gen1) (randomRs (3, 20) gen2)
    items = cycle [HealthPotion 30, EnergyPotion 40, AttackBoost 5]
    itemList = zip positions items