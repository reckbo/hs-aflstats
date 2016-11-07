.PHONY: all
all:
	stack build && stack exec afltables

.PHONY: clean
clean:
	rm -f *.out *.err core.*
