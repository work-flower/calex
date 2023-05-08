calex:
	mkdir bin
	erlc -o bin src/calex.erl
	cp bin/calex.beam test
	erlc -o test test/calex_TEST.erl
