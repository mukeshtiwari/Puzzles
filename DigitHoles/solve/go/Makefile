include ../Makefile.common

OUTPUT_D =../../build/go
SOLVER = $(OUTPUT_D)/DigitHolesSolver

$(SOLVER): DigitHolesSolver.go DigitHoles/DigitHoles.go
	export GOPATH="$$GOPATH:$(GETTC_HOME)/include/go" \
		&& go build -o $(SOLVER) DigitHolesSolver.go 

main: setup $(SOLVER)
