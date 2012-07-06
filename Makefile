.PHONY: all doc clean erlc_opts

SRCS=$(wildcard src/*.erl)
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

ifndef DEBUG
COPTS=
else
COPTS=+debug_info
endif

all: erlc_opts $(BEAMS)

ebin/%.beam: src/%.erl include/*.hrl
	erlc -I ./include -o ./ebin `cat erlc_opts` $(COPTS) $<

doc:
	@echo Making documentation...
	erl -noshell -noinput -eval 'edoc:application(erjik, ".", [])' -s erlang halt

clean:
	rm -fv -- ./doc/*.html ./doc/*.css ./doc/*.png ./doc/edoc-info \
		./ebin/*.beam ./erl_crash.dump ./otp_release ./erlc_opts
	find ./ -type f -name '*~' -print -delete

.ONESHELL:
erlc_opts:
	erl -noshell -noinput \
		-eval 'io:format("~s~n", [erlang:system_info(otp_release)])' \
		-s init stop > otp_release
	expr `cat otp_release` '<' R14B02 > /dev/null && \
		echo "-DWITHOUT_INETS_HEADER" > erlc_opts || :

