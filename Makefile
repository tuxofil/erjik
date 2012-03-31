.PHONY: all doc clean

SRCS=$(wildcard src/*.erl)
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

ifndef WITHOUT_INETS_HEADER
OPTS=
else
OPTS=-DWITHOUT_INETS_HEADER
endif

all: $(BEAMS)

ebin/%.beam: src/%.erl include/*.hrl
	erlc -I ./include -o ./ebin $(OPTS) $<

doc:
	@echo Making documentation...
	erl -noshell -noinput -eval 'edoc:application(erjik, ".", [])' -s erlang halt

clean:
	rm -f -- ./doc/*.html
	rm -f -- ./doc/*.css
	rm -f -- ./doc/*.png
	rm -f -- ./doc/edoc-info
	rm -f -- ./ebin/*.beam
	rm -f ./erl_crash.dump
	find ./ -type f -name '*~' -print -delete

