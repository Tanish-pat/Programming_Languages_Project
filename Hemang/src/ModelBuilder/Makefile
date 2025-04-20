GHC=ghc
SRC=Main.hs ModelGen.hs Registry.hs ModelRegistry.hs
OUT=backend

ODIR=build/obj
HIDIR=build/hi

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