# Makefile for quick compile run
# call make DAY=x for a specific puzzle

# See 'make help' for a list of useful targets

# ==================================================
# Constants
# ===================================================

DAY = 03
YEAR = 2025
PART = 1

OCAMLC = ocamlopt -I +str str.cmxa -I +unix unix.cmxa -O2
RUSTC = rustc --cfg 'part="$(PART)"'
CARGO = cargo
ifeq ($(YEAR),2024)
	DIR = $(YEAR)/Day$(DAY)
else ifeq ($(YEAR),2025)
	DIR = $(YEAR)/Day$(DAY)
else
	DIR = $(YEAR)/$(DAY)
endif
TARGET = $(DIR)/puzzle
EXE = $(TARGET).exe

DEBUG = on

TIME = time -f "Stats:\n  Total time: %E\n  CPU: %P\n  Memory (kB): %M"

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

ifeq ($(DEBUG),on)
	CARGO_TGT = ./target/debug/puzzle
	CARGO_BUILD = $(CARGO) build
else
	CARGO_TGT = ./target/release/puzzle
	CARGO_BUILD = $(CARGO) build --release
endif

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

ifeq ($(YEAR),2022)
EXT = ml
BEXE = ./$(EXE)
$(EXE): $(TARGET).ml
	$(call print,Compiling $< with ocaml)
	$(OCAMLC) $< -o $@

else
ifeq ($(YEAR),2023)
EXT = rs
BEXE = ./$(EXE)
$(EXE): $(CARGO_TGT)
	cp "$(CARGO_TGT)" "$(EXE)"

$(CARGO_TGT): $(TARGET).rs cargo
	$(call print,Compiling $< with rust)
	$(CARGO_BUILD)

# $(EXE): $(TARGET).rs
# 	$(call print,Compiling $< with rust)
# 	$(RUSTC) $< -o $@

else
EXT = ml
BEXE = dune exec -- advent_of_code $(YEAR) $(DAY)
$(EXE): $(TARGET).ml
	dune build

endif
endif

# =================================================
# Special Targets
# =================================================

# No display of executed commands.
$(VERBOSE).SILENT:

.PHONY: cargo
cargo:
	sed -i '8s#.*#path = "$(YEAR)/$(DAY)/puzzle.rs"#' Cargo.toml

.PHONY: compile
compile: $(EXE) ## Compile the given puzzle

.PHONY: run-test
run-test: $(EXE) ## Compile and run with test data
	$(call print,Running $< with test data)
	$(TIME) $(BEXE) --test < $(TARGET)_test.txt

.PHONY: run
run: $(EXE) ## Compile and run with input data
	$(call print,Running $< with input data)
	$(TIME) $(BEXE) < $(TARGET)_data.txt

.PHONY: clean
clean: ## Remove build files and executables
	$(call print,Deleting build files and executables)
	find . -type f -name '*.exe' -delete
	find . -type f -name '*.cmo' -delete
	find . -type f -name '*.cmi' -delete
	find . -type f -name '*.o' -delete
	find . -type f -name '*.cmx' -delete
	dune clean

.PHONY: help
help: ## Show this help
	@echo "$(color_yellow)make:$(color_reset) list of useful targets:"
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(color_blue)%-$(HELP_PADDING)s$(color_reset) %s\n", $$1, $$2}'

.PHONY: init
init: ## Create files for the new day
	$(call print,Creating files for day $(DAY))
	mkdir $(DIR)
	echo "(* ==== Puzzle $(DAY) : https://adventofcode.com/$(YEAR)/day/$(DAY) ====  *)" > $(TARGET).$(EXT)
	echo "" >> $(TARGET).$(EXT)
	echo "let preprocess input = input" >> $(TARGET).$(EXT)
	echo "" >> $(TARGET).$(EXT)
	echo "let part1 input = 0" >> $(TARGET).$(EXT)
	echo "let part2 input = 0" >> $(TARGET).$(EXT)
	echo "" >> $(TARGET).$(EXT)
	echo "let () = register_int ~year:$(YEAR) ~day:$(DAY) ~preprocess ~part1 ~part2" >> $(TARGET).$(EXT)
	touch $(TARGET)_data.txt
	touch $(TARGET)_test.txt
