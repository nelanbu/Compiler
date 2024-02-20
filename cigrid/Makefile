
all:
	ocamlbuild -use-menhir -tag explain main.native
	mv main.native cigrid
	
	
clean:
	ocamlbuild -clean
	rm -f *~ main.native \#*
