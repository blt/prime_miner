REBAR=`which rebar || which ./rebar`
DIALYZER=`which dialyzer`
ERL=`which erl`

.PHONY: all test clean deps repl release stop_node

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test:
	@$(REBAR) skip_deps=true eunit

clean:
	@rm -f erl_crash.dump rel/*primenode rel/primenode_*.tar.gz
	@rm -r rel/builds
	@$(REBAR) clean

release: deps compile
	./bin/release_maker

update: node
	escript bin/upgrade.escript primenode@127.0.0.1 primenode primenode_2

node:
	./rel/primenode/bin/primenode start &> /dev/null || true

stop_node:
	./rel/primenode/bin/primenode stop &> /dev/null || true

repl: node
	erl -name test -setcookie primenode -remsh primenode@127.0.0.1
