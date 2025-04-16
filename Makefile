# Makefile

GHC=ghc
SRC=src/Main.hs src/ModelGen.hs src/Registry.hs src/ModelRegistry.hs
OUT=backend

.PHONY: all clean run

all:
	$(GHC) -o $(OUT) $(SRC)

run: all
	./$(OUT)

clean:
	rm -f *.hi *.o src/*.dyn_hi src/*.dyn_o src/*.o src/*.hi $(OUT)