
# Variables

DIST=dist/index.js


# Targets

.PHONY: std clean

all: std

std: src
	elm make src/Main.elm --output $(DIST)

clean:
	rm -f $(DIST)
