SRC_DIR := .
HS_FILES := $(wildcard $(SRC_DIR)/*.hs)
EXEC := test
GHC_FLAGS := -Wall

ODIR := ./build/obj
HIDIR := ./build/hi

$(shell mkdir -p $(ODIR) $(HIDIR))

all: $(EXEC)

$(EXEC): $(HS_FILES)
	ghc $(GHC_FLAGS) -o $(EXEC) -odir $(ODIR) -hidir $(HIDIR) $(HS_FILES)

clean:
	rm -f $(EXEC) $(ODIR)/*.o $(HIDIR)/*.hi

rebuild: clean all