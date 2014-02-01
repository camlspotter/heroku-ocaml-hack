# use source

export PATH=/app/vendor/opam/bin:$PATH

export OPAMROOT=/app/vendor/.opam

if [ ! -d $OPAMROOT ]; then 
  mkdir /app/vendor/.opam
  yes N | opam init
fi

