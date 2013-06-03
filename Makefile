all: 
	opam update
	opam remove ocamlnet
	opam install --yes spotlib.2.2.0
	opam install --yes pcre-ocaml.7.0.2
	opam install --yes cryptokit.1.7
	opam install --yes tiny_json_conv
	opam install --yes ocurl
        opam install --yes eliom
	- echo "Freezing OPAM..."
	tar zcf opam-lib.tgz /tmp/opam-lib
	ls -l opam-lib.tgz
	opam list > opam-list.txt
	PREFIX=/tmp/opam-lib/system/bin omake
	mkdir -p target/bin/
	cp app target/bin/app

clean:
	rm -rf *.cm* *.o *.annot target app