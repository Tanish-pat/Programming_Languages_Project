# Makefile for compiling Haskell files in the src folder

GHC=ghc
SRC=src/Main.hs src/ModelGen.hs src/Registry.hs src/ModelRegistry.hs
OUT=backend

# Directories for object and interface files
ODIR=src/build/obj
HIDIR=src/build/hi

# Create the directories if they don't exist
$(shell mkdir -p $(ODIR) $(HIDIR))

# Compiler flags for object and interface files
GHC_FLAGS=-odir $(ODIR) -hidir $(HIDIR)

.PHONY: all clean run

# Default rule: compile all source files
all:
	$(GHC) -o $(OUT) $(GHC_FLAGS) $(SRC)

# Run the compiled output
run: all
	./$(OUT)

# Clean up the generated files
clean:
	rm -f $(OUT)
	rm -rf $(ODIR)/*.o $(HIDIR)/*.hi
