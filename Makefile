APP = erjik

VERSION = $(shell cat version)

.PHONY: all compile doc clean eunit dialyze all-tests \
	install-doc install-html debian-install debian-uninstall

all: $(APP)

COPTS = {outdir, ebin}, {i, \"include\"}, warn_unused_function, \
 warn_bif_clash, warn_deprecated_function, warn_obsolete_guard, verbose, \
 warn_shadow_vars, warn_export_vars, warn_unused_records, \
 warn_unused_import, warn_export_all, warnings_as_errors

ifdef DEBUG
COPTS := $(COPTS), debug_info
endif

ifdef TEST
COPTS := $(COPTS), {d, 'TEST'}
endif

ifdef TRACE
COPTS := $(COPTS), {d, 'TRACE'}
endif

OTPREL = $(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)),halt()')
ifeq ($(shell expr $(OTPREL) '<' R14B02), 1)
COPTS := $(COPTS), {d, 'WITHOUT_INETS_HEADER'}
endif

compile:
	mkdir -p ebin
	sed "s/{{VERSION}}/$(VERSION)/" src/$(APP).app.in > ebin/$(APP).app
	echo '["src/*"].' > Emakefile
	erl -noinput -eval "up_to_date=make:all([$(COPTS)]),halt()"

$(APP): compile
	zip -j $(APP) ebin/*
	echo '#!/usr/bin/env escript' > $(APP)
	echo '%%!-smp' >> $(APP)
	cat $(APP).zip >> $(APP)
	chmod 755 $(APP)

html:
	sed "s/{{VERSION}}/$(VERSION)/" doc/overview.edoc.in > doc/overview.edoc
	erl -noinput -eval \
		'edoc:application($(APP),".",[{application,$(APP)}]),halt()'

eunit:
	$(MAKE) TEST=y clean compile
	erl -noinput -pa ebin \
		-eval 'ok=eunit:test({application,$(APP)},[verbose]),halt()'

PLT = .dialyzer_plt

dialyze: $(PLT)
	dialyzer --src --plt $< -r . \
		-Wunmatched_returns -Werror_handling -Wrace_conditions
	$(MAKE) DEBUG=y clean compile
	dialyzer --plt $< -r . \
		-Wunmatched_returns -Werror_handling -Wrace_conditions

$(PLT):
	dialyzer --build_plt --output_plt $@ \
		--apps erts inets kernel stdlib crypto compiler

all-tests:
	$(MAKE) eunit
	$(MAKE) dialyze

clean:
	rm -rf -- ebin doc/*.html doc/*.css doc/*.png doc/edoc-info \
	    $(APP).zip $(APP) erl_crash.dump Emakefile doc/overview.edoc \
	    *.log *.log.* tmp_file
	find . -type f -name '*~' -delete

install-doc:
	install -m 755 --directory $(DESTDIR)
	install -m 644 README.md LICENSE $(DESTDIR)/

install-html: html
	install -m 755 --directory $(DESTDIR)
	install -m 644 doc/*.html doc/*.css doc/*.png $(DESTDIR)/

## ----------------------------------------------------------------------
## installation/deinstallation section

conf_dir    = /etc/$(APP)
sudoers_dir = /etc/sudoers.d
sbin_dir    = /usr/sbin
doc_dir     = /usr/share/doc/$(APP)
log_dir     = /var/log/$(APP)

blacklists_dir = $(conf_dir)/blacklists
regexps_dir    = $(conf_dir)/regexps
www_dir        = $(install_dir)/priv/www

INSTALL_DIR  = install --directory --mode=0755 --
INSTALL_DATA = install --mode=0644 --
INSTALL_SUDO = install --mode=0440 --
INSTALL_PROG = install --mode=0755 --

debian-install: compile
	$(INSTALL_DIR) \
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

debian-uninstall:
	rm --force --recursive --verbose -- \
	    $(DESTDIR)$(www_dir) \
	    $(DESTDIR)$(conf_dir) \
	    $(DESTDIR)$(blacklists_dir) $(DESTDIR)$(regexps_dir) \
	    $(DESTDIR)$(doc_dir) \
	    $(DESTDIR)$(log_dir) \
	    $(DESTDIR)$(sudoers_dir)/erjik \
	    $(DESTDIR)$(sbin_dir)/erjik*

