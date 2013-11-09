ERLC?= erlc

CURRENT=$(shell pwd)
BUILDDIR=$(CURRENT)/build
SUPPORT_DIR=$(CURRENT)/support
ESCRIPT=escript

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
		-d $(BUILDDIR)/thirdparty/cowboy/src \
		-d $(BUILDDIR)/thirdparty/ranch/src
	@echo "==> build coffer"
	@(cd $(BUILDDIR) && \
		$(ESCRIPT) rebar -C $(CURRENT)/rebar.config compile || exit 0)

clean:
	@rm -rf $(BUILDDIR)

distclean: clean rebarclean


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
