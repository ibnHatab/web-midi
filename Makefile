
.PHONY: test

ELM_COMPILE=elm-make --yes --warn

ELM_IO = elm-stuff/packages/maxsnew/IO/1.0.1/elm-io.sh

TESTS=$(wildcard tests/*.elm)
TESTS_JS=$(patsubst %.elm,%.elm.test.js,$(TESTS))

TARGETS=$(wildcard examples/*.elm) $(wildcard examples/apps/*.elm)
TARGETS_JS=$(patsubst %.elm,%.js,$(TARGETS))

DEMO =  demo/InputEventsFromKbd.html \
	demo/ListMIDIPorts.html	\
	demo/PerformMusic.html	\
	demo/PlayNote.html


all: compile
	~/bin/chrome-reload.sh "Main"

compile: $(TARGETS_JS)
	$(ELM_COMPILE)

%.js: %.elm
	@echo 'Compile $(ELM_COMPILE)  $<'
	$(ELM_COMPILE) $< --output $@

demo: $(DEMO)
demo/%.html: examples/%.elm
	elm-make --yes $< --output $@


test: $(TESTS_JS)

%.elm.test.js: %.elm
	elm-make --yes $< --output tests/raw.test.js
	bash $(ELM_IO) tests/raw.test.js $<.test.js
	node $<.test.js && rm $<.test.js tests/raw.test.js

clean-test:
	-@rm $(TESTS_JS) test/raw.test.js || true

clean: clean-test
	-@rm $(TARGETS_JS) || true
