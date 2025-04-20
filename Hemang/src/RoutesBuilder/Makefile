GHC=ghc
SRC=Main.hs RouteGen.hs RouteRegistry.hs
OUT=generate_routes

ODIR=build/obj
HIDIR=build/hi

$(shell mkdir -p $(ODIR) $(HIDIR))

GHC_FLAGS=-odir $(ODIR) -hidir $(HIDIR) -i.

ROUTE_TARGET_DIR=../../generated/routes

.PHONY: all clean run

all:
	@echo "Building route generator..."
	$(GHC) -o $(OUT) $(GHC_FLAGS) $(SRC)

run: all
	@echo "Running route generator..."
	@mkdir -p $(ROUTE_TARGET_DIR)
	./$(OUT)

clean:
	@echo "Cleaning build artifacts..."
	rm -f $(OUT)
	rm -rf $(ODIR)/*.o $(HIDIR)/*.hi
