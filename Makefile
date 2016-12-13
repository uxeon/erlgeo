.PHONY: all test clean
REBAR=./rebar

all: deps compile

deps:
	${REBAR} get-deps

compile:
	${REBAR} compile

recompile:
	${REBAR} skip_deps=true compile

test:
	${REBAR} skip_deps=true ct

start: compile
	erl -pa ebin/ deps/jsone/ebin -config config/sys.config

clean:
	${REBAR} clean
