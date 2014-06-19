

compile:
	rebar compile

dev: compile
	@erl -config test -pa deps/*/ebin -pa apps/*/ebin -s lager -s t
