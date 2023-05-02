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

clean:
	dune clean
