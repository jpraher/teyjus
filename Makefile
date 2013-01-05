
.PHONY: all clean teyjus deps

all:    deps teyjus

teyjus:
	omake

deps:
	$(MAKE) -C deps
