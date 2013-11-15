ERLC?= erlc

CURRENT=$(shell pwd)
BUILDDIR=$(CURRENT)/build
SUPPORT_DIR=$(CURRENT)/support
ESCRIPT=escript
DIALYZER=dialyzer

all: clean compile

compile: get-deps
	@cp -R $(CURRENT)/apps $(BUILDDIR)/apps
	@echo "==> build coffer"
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar.config compile || exit 0)

clean:
	@(test -d $(BUILDDIR) && \
		cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar.config clean || exit 0)

rel:
	@rm -rf rel/coffer
	@$(ESCRIPT) $(BUILDDIR)/rebar generate


.PHONY: rel

dev: get-deps
	@(cd $(BUILDDIR) && \
		test ! -d $(BUILDDIR)/apps && ln -s $(CURRENT)/apps . || exit 0)
	@echo "==> build coffer"
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -v -C $(CURRENT)/rebar.config compile || exit 0)

devclean:
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar.config clean || exit 0)

distclean: clean rebarclean
	@rm -rf $(BUILDDIR)

dialyze: dialyzer.plt
	@(cd $(BUILDDIR) && \
		$(DIALYZER) --plt $(CURRENT)/dialyzer.plt -I \
			apps/coffer_blobserver/ebin/ \
			apps/coffer_common/ebin/ \
			apps/coffer_server/ebin/ \
			deps/cowboy/ebin/ \
			deps/ranch/ebin/ \
			deps/jsx/ebin/ \
			deps/goldrush/ebin/ \
			deps/lager/ebin/ || exit 0)

dialyzer: dialyze

dialyzer.plt:
	$(DIALYZER) --build_plt --output_plt dialyzer.plt \
                --apps erts kernel stdlib mnesia crypto inets xmerl sasl \
                       compiler debugger ssl tools

xref:
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar_test.config xref || exit 0)

test:
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar_test.config ct && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar_test.config eunit || exit 0)

.PHONY: test


get-deps: rebar
	@mkdir -p $(BUILDDIR)
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar.config get-deps)

update-deps: rebar
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar.config update-deps)

rebar:
	@mkdir -p $(BUILDDIR)
	@(test ! -e $(CURRENT)/support/rebar/rebar && \
		echo "==> build rebar" && \
		cd $(CURRENT)/support/rebar && \
		$(ESCRIPT) bootstrap || exit 0)
	@cp $(CURRENT)/support/rebar/rebar $(BUILDDIR)/rebar

rebarclean:
	@(cd $(CURRENT)/support/rebar && \
		rm -rf rebar ebin/*.beam inttest/rt.work rt.work .test)


.PHONY: rebar
