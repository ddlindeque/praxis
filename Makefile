default: resources build/bin/praxis

resources:
	mkdir -p build
	cp samples/colourise.css build/colourise.css

build/%.o: %.hs
	mkdir -p `dirname $@`
	ghc -o $@ -c $^

build/bin/praxis: build/Common.o build/Praxis/Lexer.o build/Praxis/Colourise.o build/Args.o build/Main.o build/Lexer/Nfa.o
	mkdir -p `dirname $@`
	ghc -o $@ $^

clean:
	rm -rf build