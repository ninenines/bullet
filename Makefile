# See LICENSE for licensing information.

DIALYZER = dialyzer
REBAR = rebar

all: app

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

tests: clean app eunit ct

eunit:
	@$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .bullet_dialyzer.plt \
		--apps kernel stdlib deps/cowboy

dialyze:
	@$(DIALYZER) --src src --plt .bullet_dialyzer.plt -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true
