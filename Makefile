ERLC?= erlc

CURRENT=$(shell pwd)
BUILDDIR=$(CURRENT)/build
SUPPORT_DIR=$(CURRENT)/support
ESCRIPT=escript
DIALYZER=dialyzer

SCOPE_EXTERNAL=cf_

all: clean compile

compile: rebar
	@cp -R $(CURRENT)/apps $(BUILDDIR)/apps
	@cp -R $(CURRENT)/thirdparty $(BUILDDIR)/thirdparty
	@echo "==> fix scope"
	@$(CURRENT)/thirdparty/reltool_util/scope -s $(SCOPE_EXTERNAL) -p coffer \
		-b _original \
		-c $(BUILDDIR)/apps/ \
		-d $(BUILDDIR)/apps/coffer_blobserver/src \
		-d $(BUILDDIR)/apps/coffer_common/src \
		-d $(BUILDDIR)/thirdparty/cowboy/src \
		-d $(BUILDDIR)/thirdparty/cowboy/test \
		-d $(BUILDDIR)/thirdparty/ranch/src \
		-d $(BUILDDIR)/thirdparty/ranch/test
	@echo "==> build coffer"
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar.config compile || exit 0)

clean:
	@rm -rf $(BUILDDIR)

dev: rebar
	@mkdir -p $(BUILDDIR)
	@(cd $(BUILDDIR) && \
		test ! -d $(BUILDDIR)/apps && ln -s $(CURRENT)/apps . || exit 0)
	@cp -R $(CURRENT)/thirdparty $(BUILDDIR)/thirdparty
	@echo "==> fix scope"
	@$(CURRENT)/thirdparty/reltool_util/scope -s $(SCOPE_EXTERNAL) -p coffer \
		-b _original \
		-c $(BUILDDIR)/apps/ \
		-d $(BUILDDIR)/apps/coffer_blobserver/src \
		-d $(BUILDDIR)/apps/coffer_common/src \
		-d $(BUILDDIR)/thirdparty/cowboy/src \
		-d $(BUILDDIR)/thirdparty/cowboy/test \
		-d $(BUILDDIR)/thirdparty/ranch/src \
		-d $(BUILDDIR)/thirdparty/ranch/test
	@echo "==> build coffer"
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar.config compile || exit 0)

devclean:
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar.config clean || exit 0)

distclean: clean rebarclean

dialyze: dialyzer.plt
	@(cd $(BUILDDIR) && \
		$(DIALYZER) --plt $(CURRENT)/dialyzer.plt -I \
			apps/coffer_blobserver/ebin/ \
			apps/coffer_common/ebin/ \
			thirdparty/cowboy/ebin/ \
			thirdparty/ranch/ebin/ || exit 0)

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

rebar:
	@mkdir -p $(BUILDDIR)
	@(test ! -e $(CURRENT)/thirdparty/rebar/rebar && \
		echo "==> build rebar" && \
		cd $(CURRENT)/thirdparty/rebar && \
		$(ESCRIPT) bootstrap || exit 0)
	@cp $(CURRENT)/thirdparty/rebar/rebar $(BUILDDIR)/rebar

rebarclean:
	@(cd $(CURRENT)/thirdparty/rebar && \
		rm -rf rebar ebin/*.beam inttest/rt.work rt.work .test)


.PHONY: rebar
