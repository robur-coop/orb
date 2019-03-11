all: orb
ALWAYS:
orb: ALWAYS
	dune build src/orb.exe
	@cp _build/default/src/orb.exe $@

run: orb
	dune exec src/orb.exe

clean:
	rm -f dune-project
	dune clean

distclean:
	rm -f orb
