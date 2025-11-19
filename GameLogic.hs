-- GameLogic.hs
-- Toda la lógica del juego usando la Monada State

module GameLogic where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import GameTypes

-- | Manejar input del usuario (usando la Monada State)
handleInput :: Char -> GameM ()
handleInput 'w' = movePlayer (0, -1)
handleInput 's' = movePlayer (0, 1)
handleInput 'a' = movePlayer (-1, 0)
handleInput 'd' = movePlayer (1, 0)
handleInput 'e' = toggleForm
handleInput 'q' = attackNearby
handleInput _ = setMessage "Tecla no reconocida"

-- | Mover el jugador
movePlayer :: (Int, Int) -> GameM ()
movePlayer (dx, dy) = do
    gs <- get
    let player = gsPlayer gs
        (x, y) = pPosition player
        newPos = (x + dx, y + dy)
    
    -- Verificar si puede moverse
    canMove <- canMoveTo newPos
    
    when canMove $ do
        -- Actualizar posición
        modify $ \s -> s { gsPlayer = player { pPosition = newPos } }
        
        -- Revelar el tile
        modify $ \s -> s { gsRevealedMap = S.insert newPos (gsRevealedMap s) }
        
        -- Recoger items
        pickupItem newPos
        
        -- Verificar victoria
        when (newPos == (0, 0)) $ 
            modify $ \s -> s { gsVictory = True, gsMessage = "¡VICTORIA!" }
        
        -- Procesar turno (enemigos, efectos)
        processTurn
        
        setMessage $ "Movido a " ++ show newPos

-- | Verificar si se puede mover a una posición
canMoveTo :: Position -> GameM Bool
canMoveTo pos = do
    gs <- get
    let player = gsPlayer gs
        form = pForm player
        isWallPos = isWall pos
    
    case form of
        Ghostly -> return True  -- Fantasma atraviesa paredes
        Earthly -> return (not isWallPos)

-- | Verificar si una posición es una pared
isWall :: Position -> Bool
isWall (x, y) = 
    let wallPattern = (abs x `mod` 15 == 0 && abs y `mod` 3 /= 1) ||
                     (abs y `mod` 15 == 0 && abs x `mod` 3 /= 1)
        farFromCenter = distance (x,y) (0,0) > 3
    in wallPattern && farFromCenter
  where
    distance (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2))

-- | Cambiar de forma
toggleForm :: GameM ()
toggleForm = do
    gs <- get
    let player = gsPlayer gs
        cooldown = pFormCooldown player
    
    if cooldown > 0 then
        setMessage $ "Cooldown: " ++ show cooldown ++ " turnos"
    else do
        let currentForm = pForm player
            newForm = if currentForm == Earthly then Ghostly else Earthly
            energyCost = if newForm == Ghostly then 20 else 0
            currentEnergy = pEnergy player
        
        if currentEnergy >= energyCost then do
            modify $ \s -> s { gsPlayer = player 
                { pForm = newForm
                , pEnergy = currentEnergy - energyCost
                , pFormCooldown = 3
                } }
            setMessage $ "Cambiado a forma " ++ show newForm
        else
            setMessage "Energía insuficiente para cambiar de forma"

-- | Atacar enemigos cercanos
attackNearby :: GameM ()
attackNearby = do
    gs <- get
    let player = gsPlayer gs
        form = pForm player
    
    if form == Ghostly then
        setMessage "No puedes atacar en forma fantasmal"
    else do
        let playerPos = pPosition player
            enemies = gsEnemies gs
            nearbyEnemies = filter (\e -> isAdjacent playerPos (ePosition e)) enemies
        
        case nearbyEnemies of
            [] -> setMessage "No hay enemigos cercanos"
            (enemy:_) -> do
                let damage = pAttack player
                    newHealth = eHealth enemy - damage
                
                if newHealth <= 0 then do
                    -- Enemigo eliminado
                    modify $ \s -> s { gsEnemies = filter (/= enemy) (gsEnemies s) }
                    setMessage $ "¡Enemigo eliminado! Daño: " ++ show damage
                else do
                    -- Enemigo dañado
                    let updatedEnemy = enemy { eHealth = newHealth }
                    modify $ \s -> s { gsEnemies = updatedEnemy : filter (/= enemy) (gsEnemies s) }
                    setMessage $ "Atacaste! Daño: " ++ show damage ++ " HP restante: " ++ show newHealth

-- | Verificar si dos posiciones son adyacentes
isAdjacent :: Position -> Position -> Bool
isAdjacent (x1, y1) (x2, y2) = 
    abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1 && (x1, y1) /= (x2, y2)

-- | Recoger item
pickupItem :: Position -> GameM ()
pickupItem pos = do
    gs <- get
    let items = gsItems gs
    
    case M.lookup pos items of
        Nothing -> return ()
        Just item -> do
            applyItem item
            modify $ \s -> s { gsItems = M.delete pos (gsItems s) }

-- | Aplicar efecto de item
applyItem :: Item -> GameM ()
applyItem (HealthPotion amount) = do
    modify $ \s -> 
        let player = gsPlayer s
            newHealth = min (pMaxHealth player) (pHealth player + amount)
        in s { gsPlayer = player { pHealth = newHealth } }
    setMessage $ "¡Recogiste poción de salud! +" ++ show amount ++ " HP"

applyItem (EnergyPotion amount) = do
    modify $ \s ->
        let player = gsPlayer s
            newEnergy = min (pMaxEnergy player) (pEnergy player + amount)
        in s { gsPlayer = player { pEnergy = newEnergy } }
    setMessage $ "¡Recogiste poción de energía! +" ++ show amount ++ " EN"

applyItem (AttackBoost amount) = do
    modify $ \s ->
        let player = gsPlayer s
            newAttack = pAttack player + amount
        in s { gsPlayer = player { pAttack = newAttack } }
    setMessage $ "¡Ataque aumentado! +" ++ show amount ++ " ATK"

-- | Procesar turno (enemigos, efectos)
processTurn :: GameM ()
processTurn = do
    -- Incrementar contador de turnos
    modify $ \s -> s { gsTurn = gsTurn s + 1 }
    
    -- Reducir cooldowns
    modify $ \s ->
        let player = gsPlayer s
            newCooldown = max 0 (pFormCooldown player - 1)
        in s { gsPlayer = player { pFormCooldown = newCooldown } }
    
    -- Consumir energía si está en forma fantasmal
    gs <- get
    let player = gsPlayer gs
    when (pForm player == Ghostly) $ do
        let energyCost = 5
            currentEnergy = pEnergy player
        
        if currentEnergy <= energyCost then do
            -- Forzar volver a forma terrenal
            modify $ \s -> s { gsPlayer = player 
                { pForm = Earthly
                , pEnergy = 0
                } }
            setMessage "¡Energía agotada! Vuelves a forma terrenal"
        else
            modify $ \s -> s { gsPlayer = player 
                { pEnergy = currentEnergy - energyCost } }
    
    -- Mover y atacar con enemigos
    processEnemies
    
    -- Verificar game over
    checkGameOver

-- | Procesar acciones de enemigos
processEnemies :: GameM ()
processEnemies = do
    gs <- get
    let player = gsPlayer gs
        playerPos = pPosition player
        enemies = gsEnemies gs
    
    -- Enemigos atacan si están cerca y jugador es terrenal
    when (pForm player == Earthly) $ do
        let attackingEnemies = filter (\e -> isAdjacent playerPos (ePosition e)) enemies
        
        unless (null attackingEnemies) $ do
            let totalDamage = sum $ map eAttack attackingEnemies
            modify $ \s ->
                let p = gsPlayer s
                    newHealth = pHealth p - totalDamage
                in s { gsPlayer = p { pHealth = newHealth } }
            
            setMessage $ "¡Recibiste " ++ show totalDamage ++ " de daño!"
    
    -- Enemigos tipo Drainer drenan energía
    when (pForm player == Earthly) $ do
        let drainers = filter (\e -> eType e == Drainer && isAdjacent playerPos (ePosition e)) enemies
        
        unless (null drainers) $ do
            let energyDrain = 10 * length drainers
            modify $ \s ->
                let p = gsPlayer s
                    newEnergy = max 0 (pEnergy p - energyDrain)
                in s { gsPlayer = p { pEnergy = newEnergy } }

-- | Verificar condición de game over
checkGameOver :: GameM ()
checkGameOver = do
    gs <- get
    let player = gsPlayer gs
    when (pHealth player <= 0) $
        modify $ \s -> s { gsGameOver = True, gsMessage = "¡Has muerto!" }

-- | Establecer mensaje
setMessage :: String -> GameM ()
setMessage msg = modify $ \s -> s { gsMessage = msg }