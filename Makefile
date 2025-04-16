GHC=ghc
SRC=src/Main.hs src/ModelGen.hs src/Registry.hs src/ModelRegistry.hs
OUT=backend

ODIR=src/build/obj
HIDIR=src/build/hi

$(shell mkdir -p $(ODIR) $(HIDIR))

GHC_FLAGS=-odir $(ODIR) -hidir $(HIDIR)

.PHONY: all clean run

all:
	$(GHC) -o $(OUT) $(GHC_FLAGS) $(SRC)

run: all
	./$(OUT)

clean:
	rm -f $(OUT)
	rm -rf $(ODIR)/*.o $(HIDIR)/*.hi