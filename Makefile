all: 

#	# clean the old opam
#	opam remove --yes `opam list -i -s | sed -e 's/base-[^ ]*//g'`
#	opam repository remove myrepo 

	# download heroku custom repo
	curl -L https://github.com/camlspotter/opam-repository-heroku/archive/latest.tar.gz -s -o - | tar zxvf - -C /app/vendor

#	# add heroku custom repo
#	opam repository add heroku /app/vendor/opam-repository-heroku-latest
#	opam repository list

#	opam update

#	# install my owns
#	opam install --yes omake
#	opam install --yes spotlib
#	opam install --yes pcre-ocaml.7.0.2
#	opam install --yes cryptokit.1.7
#	opam install --yes tiny_json_conv
#	opam install --yes ocurl

#	opam install --yes dbm
#	opam install --yes eliom

	echo '#include <ndbm.h>' > hasgot.c
	echo 'int main() { (void) dbm_open("foo", 0, 0); return 0; }' >> hasgot.c
	gcc -L/app/vendor/gdbm/lib -I/app/vendor/gdbm/include -o hasgot.exe hasgot.c -lgdbm_compat -lgdbm
	@echo "Freezing OPAM..."
	tar zcf opam-lib.tgz /app/vendor/opam-lib
	ls -l opam-lib.tgz
	opam list > opam-list.txt
	PREFIX=/app/vendor/opam-lib/system/bin omake
	mkdir -p target/bin/
	cp main target/bin/main

clean:
	rm -rf *.cm* *.o *.annot target app
