include ../Makefile.common

OUTPUT_D = ../../build/haskell
SOLVER = $(OUTPUT_D)/DigitHolesSolver
COMPILER = ghc -i$(GETTC_HOME)/include/haskell -outputdir $(OUTPUT_D)

$(SOLVER): DigitHolesSolver.hs DigitHoles.hs
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)

