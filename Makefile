all: 
#	opam remove --yes `opam list -i -s | sed -e 's/base-[^ ]*//g'`
#	opam repository remove myrepo 
#	opam update
#	opam install --yes omake
#	opam install --yes spotlib
#	opam install --yes pcre-ocaml.7.0.2
#	opam install --yes cryptokit.1.7
#	opam install --yes tiny_json_conv
#	opam install --yes ocurl
	opam repository list
	- opam repository add heroku http://github.com/camlspotter/opam-repository-heroku.git
	cat /app/vendor/opam-lib/log/log-4bcc67*
	@echo "Freezing OPAM..."
	tar zcf opam-lib.tgz /app/vendor/opam-lib
	ls -l opam-lib.tgz
	opam list > opam-list.txt
	PREFIX=/app/vendor/opam-lib/system/bin omake
	mkdir -p target/bin/
	cp main target/bin/main

clean:
	rm -rf *.cm* *.o *.annot target app
