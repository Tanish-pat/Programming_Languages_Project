# Makefile to compile all Haskell files in the current directory (generated)

# Define the current directory where the .hs files are located
SRC_DIR := .
HS_FILES := $(wildcard $(SRC_DIR)/*.hs)

# Define the output executable name
EXEC := test

# Define GHC (Haskell compiler) options
GHC_FLAGS := -Wall

# Define directories for object and interface files
ODIR := ./build/obj
HIDIR := ./build/hi

# Create the directories for object and interface files if they don't exist
$(shell mkdir -p $(ODIR) $(HIDIR))

# Default rule
all: $(EXEC)

# Rule to compile all Haskell files in the current directory
$(EXEC): $(HS_FILES)
	ghc $(GHC_FLAGS) -o $(EXEC) -odir $(ODIR) -hidir $(HIDIR) $(HS_FILES)

# Rule to clean the generated executable and object files
clean:
	rm -f $(EXEC) $(ODIR)/*.o $(HIDIR)/*.hi

# Rule to recompile everything
rebuild: clean all
