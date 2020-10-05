CCL=$(shell ros -L ccl-bin run -Q -- -h 2>&1 | sed -n '1p' | cut -d " " -f 2)
MAKEFILE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
DESTDIR=$(HOME)

define ENV
env MAKEFILE_DIR="$(MAKEFILE_DIR)"
endef

bin/dw: src/main.lisp src/find.lisp src/filesystem.lisp src/utility.lisp
	mkdir -p bin
	$(ENV) $(CCL) --load build-ccl.lisp

install: bin/dw
	install -Dm755 bin/dw $(DESTDIR)/bin/dw

uninstall:
	rm -rf $(DESTDIR)/bin/dw

clean:
	rm -rf bin/dw
