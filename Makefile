REBAR=which rebar || ./rebar

all: compile
compile:
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

