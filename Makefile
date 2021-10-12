default: resources build/bin/praxis

resources:
	mkdir -p build
	cp samples/colourise.css build/colourise.css

build/%.o: %.hs
	mkdir -p `dirname $@`
	ghc -o $@ -c $^

build/bin/praxis: build/Common.o build/Praxis/ManualLexer.o build/Praxis/ManualParser.o build/Praxis/ParserCommon.o build/Praxis/Colourise.o build/Praxis/Graph.o build/Praxis/Languages/Haskell.o build/Praxis/Generate.o build/Praxis/FormatCommon.o build/Praxis/Formats/Json.o build/Praxis/Ast.o build/Args.o build/Main.o build/Lexer/Nfa.o
	mkdir -p `dirname $@`
	ghc -o $@ $^

clean:
	rm -rf build