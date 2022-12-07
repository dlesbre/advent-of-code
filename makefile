# Makefile for quick compile run
# call make DAY=x for a specific puzzle

# See 'make help' for a list of useful targets

# ==================================================
# Constants
# ===================================================

DAY = 07

OCAMLC = ocamlc
DIR = puzzles/$(DAY)
TARGET = $(DIR)/puzzle
EXE = $(TARGET).exe

# set to ON/OFF to toggle ANSI escape sequences
COLOR = ON

# Uncomment to show commands
# VERBOSE = TRUE

# padding for help on targets
# should be > than the longest target
HELP_PADDING = 15

# ==================================================
# Make code and variable setting
# ==================================================

ifeq ($(COLOR),ON)
	color_yellow = \033[93;1m
	color_orange = \033[33m
	color_red    = \033[31m
	color_green  = \033[32m
	color_blue   = \033[34;1m
	color_reset  = \033[0m
endif

define print
	@echo "$(color_yellow)$(1)$(color_reset)"
endef

# =================================================
# Default target
# =================================================

default: run-test
.PHONY: default

# =================================================
# Specific rules
# =================================================

$(EXE): $(TARGET).ml
	$(call print,Compiling $<)
	$(OCAMLC) $< -o $@

# =================================================
# Special Targets
# =================================================

# No display of executed commands.
$(VERBOSE).SILENT:

.PHONY: compile
compile: $(EXE) ## Compile the given puzzle

.PHONY: run-test
run-test: $(EXE) ## Compile and run with test data
	$(call print,Running $< with test data)
	./$(EXE) < $(TARGET)_test.txt

.PHONY: run
run: $(EXE) ## Compile and run with input data
	$(call print,Running $< with input data)
	./$(EXE) < $(TARGET)_data.txt

.PHONY: clean
clean: ## Remove build files and executables
	$(call print,Deleting build files and executables)
	find . -type f -name '*.exe' -delete
	find . -type f -name '*.cmo' -delete
	find . -type f -name '*.cmi' -delete

.PHONY: help
help: ## Show this help
	@echo "$(color_yellow)make:$(color_reset) list of useful targets:"
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(color_blue)%-$(HELP_PADDING)s$(color_reset) %s\n", $$1, $$2}'

.PHONY: init
init: ## Create files for the new day
	$(call print,Creating files for day $(DAY))
	mkdir $(DIR)
	echo "(* ==== Puzzle $(DAY) : https://adventofcode.com/2022/day/$(DAY) ==== *)" > $(TARGET).ml
	touch $(TARGET)_data.txt
	touch $(TARGET)_test.txt
