DEPS = $(CURDIR)/deps
DIALYZER_DEPS = deps/chef_authn/ebin \
                deps/chef_db/ebin \
                deps/chef_index/ebin \
                deps/chef_objects/ebin \
                deps/ej/ebin \
                deps/lager/ebin \
                deps/mini_s3/ebin \
                deps/pooler/ebin \
                deps/sqerl/ebin \
                deps/stats_hero/ebin \
                deps/ibrowse/ebin \
                deps/webmachine/ebin \
                deps/jiffy/ebin

DEPS_PLT = chef_wm.plt

all: compile eunit dialyzer

clean:
	@rebar clean

compile: $(DEPS)
	@rebar compile

$(DEPS):
	@rebar get-deps

distclean:
	@rm -rf deps $(DEPS_PLT)
	@rebar skip_deps=true clean

eunit:
	@rebar skip_deps=true eunit

test: eunit

dialyzer: $(DEPS_PLT)
	@dialyzer -Wrace_conditions -Wunderspecs --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)


doc:
	@rebar doc skip_deps=true

.PHONY: doc test eunit distclean compile clean all dialyzer

shell: compile
# You often want *rebuilt* rebar tests to be available to the
# # shell you have to call eunit (to get the tests
# # rebuilt). However, eunit runs the tests, which probably
# # fails (thats probably why You want them in the shell). This
# # runs eunit but tells make to ignore the result.
	- @rebar skip_deps=true eunit
	@erl -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

