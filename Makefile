all :
	ocamlc unix.cma str.cma abr.ml -o abr

clean :
	rm -rf *.cmi 
