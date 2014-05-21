all:
.PHONY: all

DIALYZER_PLT = .dialyzer_plt

$(DIALYZER_PLT):
	dialyzer --build_plt --output_plt $@ --apps erts kernel stdlib crypto compiler syntax_tools

dialyze: $(DIALYZER_PLT)
	dialyzer --plt $< -Werror_handling --src -r src
.PHONY: dialyze
