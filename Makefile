CC = gfortran
CPP = $(CC) -cpp
CCFLAGS = -g -Wall
LDFLAGS = `pkg-config --cflags --libs gtk-4-fortran`

# Makefile settings - Can be customized.
APPNAME = bfcv
EXT = .f90
SRCDIR = src
OBJDIR = obj

SRC = $(wildcard $(SRCDIR)/*$(EXT))
OBJ = $(SRC:$(SRCDIR)/%$(EXT)=$(OBJDIR)/%.o)
DEP = $(OBJ:$(OBJDIR)/%.o=%.d)
RM = rm

all: $(APPNAME)

# Builds the app
$(APPNAME): $(OBJ)
	$(CC) $(CCFLAGS) -o $@ $^ $(LDFLAGS)

# Creates the dependecy rules
%.d: $(SRCDIR)/%$(EXT)
	@$(CPP) $(CCFLAGS) $< -MM -MT $(@:%.d=$(OBJDIR)/%.o) >$@ $(LDFLAGS)

# Includes all .h files
-include $(DEP)

# Building rule for .o files and its .c/.cpp in combination with all .h
$(OBJDIR)/%.o: $(SRCDIR)/%$(EXT)
	$(CC) $(CCFLAGS) -o $@ -c $< $(LDFLAGS)

# Cleans complete project
.PHONY: clean
clean:
	$(RM) $(OBJ) $(DEP) $(APPNAME)

# Runs project
run: all
	./$(APPNAME)
