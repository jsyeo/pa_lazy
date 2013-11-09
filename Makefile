pa_lazy.cmo:
	ocamlbuild -use-ocamlfind -pkg compiler-libs -lib ocamlcommon -tag annot syntax/pa_lazy.cmo

oasis_install:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build
	ocaml setup.ml -uninstall
	ocaml setup.ml -install
