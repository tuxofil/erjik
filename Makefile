APP = erjik
VERSION = `cat version`

.PHONY: all compile doc test eunit dialyze clean

all: compile doc

COPTS := warn_unused_function, warn_bif_clash, warn_deprecated_function, \
	warn_obsolete_guard, warn_shadow_vars, warn_export_vars, \
	warn_unused_records, warn_unused_import
ifdef DEBUG
COPTS := $(COPTS), debug_info
endif
ifdef TEST
COPTS := $(COPTS), {d,'TEST',true}
endif
OTP_RELEASE = $(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)),halt()')
ifeq ($(shell expr $(OTP_RELEASE) '<' R14B02),1)
COPTS := $(COPTS), {d,'WITHOUT_INETS_HEADER',true}
endif

compile: ebin/$(APP).app
	sed "s/{{COPTS}}/$(COPTS)/" Emakefile.src > Emakefile
	erl -noshell -eval 'up_to_date=make:all(),halt()'

ebin/$(APP).app: src/$(APP).app.src version ebin
	sed s/{{VERSION}}/$(VERSION)/ $< > $@

ebin:
	mkdir --parents $@

doc: doc/overview.edoc
	erl -noshell -eval 'edoc:application($(APP),".",[{application,$(APP)}]),halt()'

doc/overview.edoc: doc/overview.edoc.src version
	sed s/{{VERSION}}/$(VERSION)/ $< > $@

test:
	$(MAKE) TEST=yes clean compile eunit
	$(MAKE) DEBUG=yes clean compile dialyze

eunit:
	erl -noshell -pa ebin -eval 'ok=eunit:test({application,$(APP)},[verbose]),halt()'

dialyze: .dialyzer_plt
	dialyzer --plt $< -r . -Wunmatched_returns -Werror_handling
	dialyzer --src --plt $< -r . -Wunmatched_returns -Werror_handling

.dialyzer_plt:
	dialyzer --build_plt --output_plt $@ \
		--apps erts kernel stdlib crypto compiler inets

clean:
	rm --force -- doc/*.html doc/*.css doc/*.png doc/edoc-info doc/*.edoc \
		ebin/*.app ebin/*.beam erl_crash.dump Emakefile
	find . -type f -name '*~' -delete

