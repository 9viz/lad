PREFIX  = $(HOME)/usr/local
BINPATH = $(PREFIX)/bin
BIN     = lad

build:
	@raco exe --vv lad.rkt -o $(BIN)

install: build
	@cp $(BIN) $(DESTDIR)/$(BINPATH)/$(BIN)

uninstall: build
	@rm $(DESTDIR)/$(BINPATH)/$(BIN)

clean:
	@rm $(BIN)

test:
	@racket lad.rkt

.PHONY: install clean build
