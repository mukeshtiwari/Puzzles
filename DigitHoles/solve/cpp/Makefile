include ../Makefile.common

OUTPUT_D =../../build/cpp
SOLVER = $(OUTPUT_D)/DigitHolesSolver
COMPILER = g++ -std=gnu++0x -I$(GETTC_HOME)/include/cpp

$(SOLVER): DigitHolesSolver.cpp DigitHoles.cpp
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)
