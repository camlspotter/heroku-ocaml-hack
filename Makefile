all: 

#	# clean the old opam
#	opam remove --yes `opam list -i -s | sed -e 's/base-[^ ]*//g'`
#	opam repository remove myrepo 

	# download heroku custom repo
	opam repository remove heroku # the archive contains old heroku repo
	curl -L https://github.com/camlspotter/opam-repository-heroku/archive/latest.tar.gz -s -o - | tar zxf - -C /app/vendor

#	# add heroku custom repo
	opam repository add heroku /app/vendor/opam-repository-heroku-latest
	opam repository list

	opam update

#	# install my owns
#	opam install --yes omake
#	opam install --yes spotlib
#	opam install --yes pcre-ocaml.7.0.2
#	opam install --yes cryptokit.1.7
#	opam install --yes tiny_json_conv
#	opam install --yes ocurl

#	opam install --yes eliom

	-opam install --yes dbm
	cat /app/vendor/opam-lib/system/build/dbm.1.0/dbm-2913ab*

	@echo "Freezing OPAM..."
	tar zcf opam-lib.tgz /app/vendor/opam-lib
	ls -l opam-lib.tgz
	opam list > opam-list.txt
	PREFIX=/app/vendor/opam-lib/system/bin omake
	mkdir -p target/bin/
	cp main target/bin/main

clean:
	rm -rf *.cm* *.o *.annot target app
