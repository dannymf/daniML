build:
	dune build

utop:
	dune utop src

.PHONY: repl
repl:
	dune exec src/repl/repl.exe

.PHONY: test
test:
	dune exec test/main.exe

.PHONY: run
run:
	dune exec test/run.exe

.PHONY: interpret
interpret:
	dune exec test/interpret.exe $(n) $(fname)

clean:
	dune clean
