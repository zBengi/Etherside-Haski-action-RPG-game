# Etherside

**Action RPG 2D con Sistema de Doble Forma**

Proyecto desarrollado para INFO188 - Tarea 1: Implementación de videojuego tipo "action-RGP" en Haskell usando la Monada State

---

## 📋 Descripción del Juego

**Etherside** es un action-RPG 2D por terminal donde controlas a un personaje capaz de alternar entre dos formas: **Terrenal** y **Fantasmal**. Cada forma tiene habilidades y limitaciones únicas que debes usar estratégicamente para superar obstáculos, derrotar enemigos y alcanzar el **Núcleo del Espíritu** ubicado en el centro del mapa (coordenadas 0,0).

### 🎮 Características Principales

1. **Sistema de Doble Forma**
   - **Forma Terrenal (☺)**: Puede atacar enemigos pero no atraviesa paredes
   - **Forma Fantasmal (Ⓢ)**: Atraviesa obstáculos pero no puede atacar y consume energía constantemente

2. **Mapa con Revelación Progresiva**
   - El mapa se revela mientras exploras
   - Si mueres, el progreso del mapa revelado se conserva
   - La niebla oculta áreas no visitadas

3. **Sistema de Energía**
   - Cambiar a forma fantasmal cuesta 20 de energía
   - Mantenerse en forma fantasmal consume 5 de energía por turno
   - Si la energía llega a 0, vuelves automáticamente a forma terrenal

4. **Enemigos Variados**
   - **Walker (E)**: Enemigos básicos que atacan en cuerpo a cuerpo
   - **Drainer (E)**: Enemigos que además drenan tu energía

5. **Sistema de Items**
   - **♥ Poción de Salud**: Restaura HP
   - **⚡ Poción de Energía**: Restaura energía
   - **⚔ Boost de Ataque**: Aumenta tu daño permanentemente

---

## 🎯 Objetivo del Juego

Encuentra el **Núcleo del Espíritu (★)** ubicado en las coordenadas (0, 0) del mapa. Para lograrlo deberás:

- Explorar el mapa usando ambas formas estratégicamente
- Gestionar tu energía cuidadosamente
- Combatir o evadir enemigos
- Recoger items para mejorar tus capacidades
- Atravesar obstáculos que solo pueden superarse en forma fantasmal

---

## 🕹️ Controles

| Tecla | Acción |
|-------|--------|
| **W** | Mover arriba |
| **S** | Mover abajo |
| **A** | Mover izquierda |
| **D** | Mover derecha |
| **E** | Cambiar forma (Terrenal ↔ Fantasmal) |
| **Q** | Atacar enemigos cercanos (solo en forma terrenal) |

---

## 🛠️ Requisitos Técnicos Cumplidos

### ✅ Requisitos Obligatorios

1. **Uso de la Monada State**: Todo el juego usa `Control.Monad.State` para gestionar el estado
2. **Juego 2D por terminal**: Renderizado completo en terminal con caracteres Unicode
3. **Control interactivo con teclado**: Input en tiempo real con WASD + E + Q
4. **Items que afectan al personaje**: Pociones de salud, energía y boosts de ataque
5. **Objetivo claro**: Encontrar el Núcleo del Espíritu
6. **Obstáculos y enemigos**: Paredes, enemigos Walker y Drainer

### ✅ Requisitos de Estructura

- **Main.hs**: Loop principal del juego
- **GameTypes.hs**: Módulo con todos los tipos de datos
- **GameLogic.hs**: Módulo con la lógica del juego usando State
- **Makefile**: Compilación y ejecución automatizada

---

## 🏗️ Arquitectura del Código

### Uso de la Monada State

El juego hace uso extensivo de la Monada State a través del tipo `GameM`:

```haskell
type GameM a = State GameState a
```

**Ejemplos de uso:**

```haskell
-- Mover al jugador (modifica el estado)
movePlayer :: (Int, Int) -> GameM ()
movePlayer (dx, dy) = do
    gs <- get
    let player = gsPlayer gs
        newPos = (x + dx, y + dy)
    modify $ \s -> s { gsPlayer = player { pPosition = newPos } }
```

```haskell
-- Cambiar de forma (lee y modifica el estado)
toggleForm :: GameM ()
toggleForm = do
    gs <- get
    let player = gsPlayer gs
    modify $ \s -> s { gsPlayer = player { pForm = Ghostly } }
```

```haskell
-- Aplicar item (encadena operaciones sobre el estado)
applyItem :: Item -> GameM ()
applyItem (HealthPotion amount) = do
    modify $ \s -> 
        let player = gsPlayer s
            newHealth = min (pMaxHealth player) (pHealth player + amount)
        in s { gsPlayer = player { pHealth = newHealth } }
    setMessage $ "¡Poción de salud! +" ++ show amount
```

### Estado del Juego (GameState)

```haskell
data GameState = GameState
    { gsPlayer :: Player              -- Estado del jugador
    , gsEnemies :: [Enemy]            -- Lista de enemigos
    , gsItems :: M.Map Position Item  -- Items en el mapa
    , gsRevealedMap :: S.Set Position -- Mapa revelado (persiste tras muerte)
    , gsGameOver :: Bool              -- Condición de derrota
    , gsVictory :: Bool               -- Condición de victoria
    , gsMessage :: String             -- Mensaje al jugador
    , gsTurn :: Int                   -- Contador de turnos
    , gsRng :: StdGen                 -- Generador aleatorio
    }
```

---

## 📦 Compilación e Instalación

### Requisitos Previos

- GHC (Glasgow Haskell Compiler) 8.10 o superior
- Make

### Compilar el Proyecto

```bash
# Opción 1: Usar Makefile
make

# Opción 2: Compilar manualmente
ghc -O2 -o etherside Main.hs
```

### Ejecutar el Juego

```bash
# Opción 1: Usar Makefile
make run

# Opción 2: Compilar y ejecutar
make play

# Opción 3: Ejecutar directamente
./etherside
```

### Limpiar Archivos Generados

```bash
make clean
```

---

## 🎨 Elementos Visuales del Juego

### Símbolos del Mapa

| Símbolo | Significado |
|---------|-------------|
| **☺** | Jugador en forma terrenal |
| **Ⓢ** | Jugador en forma fantasmal |
| **E** | Enemigo (Walker o Drainer) |
| **♥** | Poción de salud |
| **⚡** | Poción de energía |
| **⚔** | Boost de ataque |
| **██** | Pared (no atravesable en forma terrenal) |
| **★** | Núcleo del Espíritu (objetivo) |
| **·** | Espacio vacío revelado |
| **░░** | Niebla (área no explorada) |

### Interfaz de Usuario

```
╔══════════════════════════════════════════════════════════╗
║ ETHERSIDE                      Forma: TERRENAL          ║
╠══════════════════════════════════════════════════════════╣
║ HP: [████████████████████░░░] ║
║ EN: [██████████████░░░░░░░░░] ║
║ ATK: 15 | Pos: (12, 8)                                  ║
╠══════════════════════════════════════════════════════════╣
║   [MAPA 11x11 CON JUGADOR, ENEMIGOS, ITEMS]             ║
╠══════════════════════════════════════════════════════════╣
║ Controles: WASD=Mover | E=Cambiar Forma | Q=Atacar      ║
║ Recogiste poción de energía! +40 EN                     ║
╚══════════════════════════════════════════════════════════╝
```

---

## 🧠 Estrategias y Mecánicas

### Gestión de Energía

- **Planifica tus cambios de forma**: Cada cambio cuesta 20 de energía
- **No abuses del modo fantasma**: Consume 5 energía por turno
- **Recolecta pociones de energía**: Son vitales para atravesar secciones complicadas

### Combate

- **Forma terrenal para combatir**: Solo puedes atacar en esta forma
- **Forma fantasmal para escapar**: Los enemigos no te dañan directamente
- **Boost de ataque**: Recógelos para ser más efectivo contra enemigos

### Exploración

- **El mapa revelado persiste**: Si mueres, conservas tu conocimiento del mundo
- **Usa la forma fantasmal para explorar**: Atraviesa paredes para descubrir nuevas áreas
- **Enemigos Drainer son peligrosos**: Drenan energía incluso en forma terrenal

---

## 🔧 Decisiones de Diseño

### 1. Por qué la Monada State

La Monada State es perfecta para este juego porque:

- **Gestiona estado complejo**: Jugador, enemigos, items, mapa revelado
- **Encadena operaciones**: Múltiples modificaciones al estado en secuencia
- **Código más limpio**: Evita pasar el estado manualmente entre funciones
- **Inmutabilidad funcional**: El estado se modifica de forma segura

### 2. Sistema de Doble Forma

Este sistema añade profundidad estratégica:

- **Decisiones tácticas**: ¿Atacar o evadir?
- **Gestión de recursos**: La energía es limitada
- **Exploración creativa**: Encuentra rutas alternativas

### 3. Mapa Persistente

Conservar el mapa revelado tras morir:

- **Reduce frustración**: No pierdes todo tu progreso
- **Fomenta exploración**: Cada muerte aporta conocimiento
- **Sensación de progreso**: Gradualmente conoces el mundo

---

## 👥 Equipo de Desarrollo

- **Integrante**: [Benjamin-Martinez] 
- **Integrante**: [Ninoska-Toledo]

---

## 📚 Referencias

- [Haskell State Monad](https://wiki.haskell.org/State_Monad)
- [Learn You a Haskell - State Monad](http://learnyouahaskell.com/for-a-few-monads-more#state)
- [Real World Haskell - Monads](http://book.realworldhaskell.org/read/monads.html)

---

## 🚀 Mejoras Futuras Posibles

- [ ] Más tipos de enemigos con comportamiento IA
- [ ] Sistema de guardado de partida
- [ ] Mapas procedurales aleatorios
- [ ] Efectos de sonido ASCII
- [ ] Modo multijugador local
- [ ] Jefes finales con mecánicas únicas
- [ ] Sistema de habilidades desbloqueables

---

**¡Disfruta jugando Etherside!** 🎮👻