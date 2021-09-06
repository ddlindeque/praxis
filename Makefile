main: praxis



praxis: main.hs
	ghc -o $@ $<