all: 
	opam update
	opam remove ocamlnet
	opam install --yes spotlib.2.2.0
	opam install --yes pcre-ocaml.7.0.2
	opam install --yes cryptokit.1.7
	opam install --yes tiny_json_conv
	opam install --yes ocurl
	- echo "Freezing OPAM..."
	tar zcf opam-lib_system.tgz /tmp/opam-lib/system
	ls -l opam-lib_system.tgz
	omake
	mkdir -p target/bin/
	cp app target/bin/app

clean:
	rm -rf *.cm* *.o *.annot target app