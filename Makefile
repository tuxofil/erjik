.PHONY: all doc clean erlc_opts

SRCS=$(wildcard src/*.erl)
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

all: erlc_opts $(BEAMS)

ebin/%.beam: src/%.erl include/*.hrl
	erlc -I ./include -o ./ebin `cat erlc_opts` $<

doc:
	@echo Making documentation...
	erl -noshell -noinput -eval 'edoc:application(erjik, ".", [])' -s erlang halt

clean:
	rm -f -- ./doc/*.html
	rm -f -- ./doc/*.css
	rm -f -- ./doc/*.png
	rm -f -- ./doc/edoc-info
	rm -f -- ./ebin/*.beam
	rm -f -- ./erl_crash.dump
	rm -f -- ./otp_release
	rm -f -- ./erlc_opts
	find ./ -type f -name '*~' -print -delete

.ONESHELL:
erlc_opts:
	erl -noshell -noinput \
		-eval 'io:format("~s~n", [erlang:system_info(otp_release)])' \
		-s init stop > otp_release
	expr `cat otp_release` '<' R14B02 > /dev/null && \
		echo "-DWITHOUT_INETS_HEADER" > erlc_opts || :

