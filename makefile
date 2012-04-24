REBAR=`which rebar` || ./rebar

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

ct:
	@$(REBAR) skip_deps=true ct

eunit:
	@$(REBAR) skip_deps=true eunit

test: compile eunit ct

clean:
	@$(REBAR) clean
