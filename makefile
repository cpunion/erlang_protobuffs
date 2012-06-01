REBAR=`which rebar` || ./rebar

all: get-deps compile escriptize

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

escriptize:
	@$(REBAR) escriptize

ct:
	@$(REBAR) skip_deps=true ct

eunit:
	@$(REBAR) skip_deps=true eunit

test: compile eunit ct

clean:
	@$(REBAR) clean

check: compile xref dialyzer

xref:
	@$(REBAR) skip_deps=true xref

APPS = kernel stdlib sasl erts syntax_tools compiler
COMBO_PLT = $(HOME)/.protobuffs_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) ebin


build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return -Wunmatched_returns -Werror_handling -Wrace_conditions --plt $(COMBO_PLT) ebin deps/getopt/ebin | \
		fgrep -v -f dialyzer.ignore-warnings

cleanplt:
	@echo
	@echo "Are you sure? It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
