all: 

	PREFIX=/app/vendor/opam-lib/system/bin omake
	mkdir -p target/bin/
	cp main target/bin/main

        mkdir/app/opam
        cp -a /app/vendor/opam-lib/system opam

clean:
	rm -rf *.cm* *.o *.annot target app
