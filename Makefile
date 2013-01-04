APP = erjik
VERSION = $(shell cat version)

.PHONY: all compile doc test eunit dialyze clean install uninstall

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

## ----------------------------------------------------------------------
## installation/deinstallation section

conf_dir    = /etc/$(APP)
sudoers_dir = /etc/sudoers.d
sbin_dir    = /usr/sbin
install_dir = /usr/lib/erlang/lib/$(APP)-$(VERSION)
doc_dir     = /usr/share/doc/$(APP)
log_dir     = /var/log/$(APP)

blacklists_dir = $(conf_dir)/blacklists
regexps_dir    = $(conf_dir)/regexps
www_dir        = $(install_dir)/priv/www

INSTALL_DIR  = install --directory --mode=0755 --
INSTALL_DATA = install --mode=0644 --
INSTALL_SUDO = install --mode=0440 --
INSTALL_PROG = install --mode=0755 --

install: compile
	$(INSTALL_DIR) \
	    $(DESTDIR)$(install_dir)/ebin \
	    $(DESTDIR)$(install_dir)/priv \
	    $(DESTDIR)$(www_dir) \
	    $(DESTDIR)$(conf_dir) \
	    $(DESTDIR)$(blacklists_dir) $(DESTDIR)$(regexps_dir) \
	    $(DESTDIR)$(sudoers_dir) \
	    $(DESTDIR)$(sbin_dir) \
	    $(DESTDIR)$(doc_dir) \
	    $(DESTDIR)$(log_dir)
	cat $(APP).conf.template | \
	    sed 's@{{VERSION}}@$(VERSION)@g' | \
	    sed 's@{{BLACKLISTS}}@$(blacklists_dir)@g' | \
	    sed 's@{{REGEXPS}}@$(regexps_dir)@g' | \
	    sed 's@{{WWW_ROOT}}@$(www_dir)@g' | \
	    $(INSTALL_DATA) /dev/stdin $(DESTDIR)$(conf_dir)/$(APP).conf
	$(INSTALL_DATA) ebin/*.beam ebin/*.app $(DESTDIR)$(install_dir)/ebin
	$(INSTALL_DATA) priv/www/*.html $(DESTDIR)$(www_dir)
	$(INSTALL_DATA) priv/blacklists/* $(DESTDIR)$(blacklists_dir)
	$(INSTALL_DATA) priv/regexps/* $(DESTDIR)$(regexps_dir)
	$(INSTALL_DATA) README.md LICENSE $(DESTDIR)$(doc_dir)
	cat sudoers.template | \
	    sed 's@{{SBIN_DIR}}@$(sbin_dir)@g' | \
	    $(INSTALL_SUDO) /dev/stdin $(DESTDIR)$(sudoers_dir)/erjik
	cat scripts/erjik | \
	    sed 's@{{CONF_DIR}}@$(conf_dir)@g' | \
	    sed 's@{{LOG_DIR}}@$(log_dir)@g' | \
	    $(INSTALL_PROG) /dev/stdin $(DESTDIR)$(sbin_dir)/erjik
	$(INSTALL_PROG) \
	    scripts/erjik-hup scripts/erjik-ping \
	    scripts/erjik-remsh scripts/erjik-stop \
	    $(DESTDIR)$(sbin_dir)
	$(INSTALL_DATA) scripts/erjik-functions $(DESTDIR)$(sbin_dir)

uninstall:
	rm --force --recursive --verbose -- \
	    $(DESTDIR)$(install_dir) \
	    $(DESTDIR)$(www_dir) \
	    $(DESTDIR)$(conf_dir) \
	    $(DESTDIR)$(blacklists_dir) $(DESTDIR)$(regexps_dir) \
	    $(DESTDIR)$(doc_dir) \
	    $(DESTDIR)$(log_dir) \
	    $(DESTDIR)$(sudoers_dir)/erjik \
	    $(DESTDIR)$(sbin_dir)/erjik*

