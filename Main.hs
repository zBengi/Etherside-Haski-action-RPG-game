-- Main.hs
-- Etherside - Action RPG 2D en Terminal
-- Usa la Monada State para gestionar el estado del juego

module Main where

import Control.Monad.State
import System.IO
import System.Random
import qualified Data.Map as M
import qualified Data.Set as S
import GameTypes
import GameLogic

-- | Función principal del juego
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    putStrLn "=== ETHERSIDE ==="
    putStrLn "Presiona cualquier tecla para comenzar..."
    _ <- getChar
    gen <- newStdGen
    let initialState = initGameState gen
    gameLoop initialState

-- | Loop principal del juego usando la Monada State
gameLoop :: GameState -> IO ()
gameLoop gs = do
    clearScreen
    renderGame gs
    
    if gsGameOver gs then do
        putStrLn "\n¡HAS SIDO DERROTADO!"
        putStrLn $ "Mapa revelado: " ++ show (S.size (gsRevealedMap gs)) ++ " tiles"
        putStrLn "\n¿Reiniciar? (s/n)"
        choice <- getChar
        if choice == 's' || choice == 'S'
            then do
                newGen <- newStdGen
                let newGs = resetGameState gs newGen
                gameLoop newGs
            else putStrLn "\n¡Gracias por jugar!"
    else if gsVictory gs then do
        putStrLn "\n¡VICTORIA! ¡Has encontrado el Núcleo del Espíritu!"
        putStrLn $ "Tiles explorados: " ++ show (S.size (gsRevealedMap gs))
        putStrLn "\n¡Gracias por jugar!"
    else do
        input <- getChar
        let newGs = execState (handleInput input) gs
        gameLoop newGs

-- | Limpiar la pantalla (compatible con Windows/Linux)
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Renderizar el estado del juego
renderGame :: GameState -> IO ()
renderGame gs = do
    putStrLn "╔══════════════════════════════════════════════════════════╗"
    putStrLn $ "║ ETHERSIDE                      Forma: " ++ formName ++ "       ║"
    putStrLn "╠══════════════════════════════════════════════════════════╣"
    putStrLn $ "║ HP: " ++ showBar (pHealth player) (pMaxHealth player) 20 ++ " ║"
    putStrLn $ "║ EN: " ++ showBar (pEnergy player) (pMaxEnergy player) 20 ++ " ║"
    putStrLn $ "║ ATK: " ++ show (pAttack player) ++ " | Pos: " ++ show (pPosition player) ++ "           ║"
    putStrLn "╠══════════════════════════════════════════════════════════╣"
    mapM_ putStrLn mapLines
    putStrLn "╠══════════════════════════════════════════════════════════╣"
    putStrLn "║ Controles: WASD=Mover | E=Cambiar Forma | Q=Atacar      ║"
    putStrLn $ "║ " ++ lastMsg ++ replicate (56 - length lastMsg) ' ' ++ " ║"
    putStrLn "╚══════════════════════════════════════════════════════════╝"
  where
    player = gsPlayer gs
    formName = case pForm player of
        Earthly -> "TERRENAL"
        Ghostly -> "FANTASMAL"
    lastMsg = take 56 (gsMessage gs)
    mapLines = renderMap gs

-- | Mostrar barra de progreso
showBar :: Int -> Int -> Int -> String
showBar current maxVal width = 
    let filled = (current * width) `div` maxVal
        empty = width - filled
    in "[" ++ replicate filled '█' ++ replicate empty '░' ++ "]"

-- | Renderizar el mapa
renderMap :: GameState -> [String]
renderMap gs = map renderRow [minY..maxY]
  where
    player = gsPlayer gs
    playerPos = pPosition player
    revealed = gsRevealedMap gs
    enemies = gsEnemies gs
    items = gsItems gs
    
    viewRadius = 5
    (px, py) = playerPos
    minX = px - viewRadius
    maxX = px + viewRadius
    minY = py - viewRadius
    maxY = py + viewRadius
    
    renderRow y = "║ " ++ concatMap (\x -> renderTile (x, y)) [minX..maxX] ++ " ║"
    
    renderTile pos
        | pos == playerPos = case pForm player of
            Earthly -> "☺ "
            Ghostly -> "Ⓢ "
        | pos `elem` map ePosition enemies = "E "
        | pos `M.member` items = renderItem (items M.! pos)
        | not (pos `S.member` revealed) = "░░"
        | isWall pos = "██"
        | isGoal pos = "★ "
        | otherwise = "· "
    
    renderItem (HealthPotion _) = "♥ "
    renderItem (EnergyPotion _) = "⚡"
    renderItem (AttackBoost _) = "⚔ "
    
    isWall (x, y) = 
        let wallPattern = (abs x `mod` 15 == 0 && abs y `mod` 3 /= 1) ||
                         (abs y `mod` 15 == 0 && abs x `mod` 3 /= 1)
        in wallPattern && distance (x,y) (0,0) > 3
    
    isGoal pos = pos == (0, 0)
    
    distance (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2))