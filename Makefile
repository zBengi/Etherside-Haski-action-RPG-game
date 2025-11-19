# Makefile para Etherside
# Compilación del proyecto Haskell

# Compilador
GHC = ghc
GHCFLAGS = -O2 -threaded -rtsopts -package mtl

# Archivos fuente
SOURCES = Main.hs GameTypes.hs GameLogic.hs
EXECUTABLE = etherside

# Target principal
all: $(EXECUTABLE)

# Compilar el ejecutable
$(EXECUTABLE): $(SOURCES)
	$(GHC) $(GHCFLAGS) -o $(EXECUTABLE) Main.hs

# Limpiar archivos generados
clean:
	rm -f *.hi *.o $(EXECUTABLE)
	rm -rf dist dist-newstyle

# Ejecutar el juego
run: $(EXECUTABLE)
	./$(EXECUTABLE)

# Compilar y ejecutar
play: all run

# Verificar sintaxis sin compilar completamente
check:
	$(GHC) -fno-code $(SOURCES)

# Información de ayuda
help:
	@echo "Makefile para Etherside"
	@echo ""
	@echo "Targets disponibles:"
	@echo "  make          - Compilar el proyecto"
	@echo "  make run      - Ejecutar el juego"
	@echo "  make play     - Compilar y ejecutar"
	@echo "  make clean    - Limpiar archivos generados"
	@echo "  make check    - Verificar sintaxis"
	@echo "  make help     - Mostrar esta ayuda"

.PHONY: all clean run play check help