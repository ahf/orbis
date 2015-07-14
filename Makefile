REBAR = ./rebar3

all: compile

compile:
	@$(REBAR) compile

rel:
	@$(REBAR) release

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer

check:
	@$(REBAR) do ct -v, cover -v

shell:
	@$(REBAR) shell

.PHONY: compile rel clean dialyzer check shell
