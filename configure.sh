#!/bin/sh
set -e

unset cflags
test $(uname -m) = "x86_64" && buildtype=native || buildtype=release

usage () {
    echo "$1"
    cat 1>&2 <<EOF
usage: $0 [-F] [-b build-type] [-O] [-n] path-to-mupdf
options:
 -F: use fontconfig
 -b: MuPDF's build type [default native]
 -O: use ocamlfind for lablGL discovery
 -n: use native OCaml compiler (bytecode otherwise)

 build-type = debug|release|native
EOF
    exit $2
}

while getopts nFb:O opt; do
    case $opt in
        F) fontconfig=true; cflags="$cflags -DUSE_FONTCONFIG";;
        b) buildtype="$OPTARG";;
        O) ocamlfind=true;;
        n) native=true;;
        ?) usage "" 0;;
    esac
done
shift $((OPTIND-1))

mudir="$1"

pkgs="openssl"
test $fontconfig && pkgs="$pkgs fontconfig" || true
pwd=$(pwd -P)

expr >/dev/null "$0" : "/.*" && {
    path="$0"
    builddir="$pwd"
    helpcmdl=" -f $(dirname $path)/build.ninja"
} || {
    path="$pwd/$0"
    builddir="build"
    helcmdl=""
    mkdir -p $builddir
}
builddir=$(cd $builddir >/dev/null $builddir && pwd -P)

libs="$(pkg-config --libs $pkgs)"

test $ocamlfind && {
    lablgldir="$(ocamlfind query lablgl)" || exit 1
    lablglcflags="-I $lablgldir"
} || {
    lablglcflags="-I +lablGL"
}

(
    if test $(uname -m) = "x86_64"; then
        cflags="$cflags -fPIC"
    else
        if test $buildtype = "native"; then
            echo "native build type does not work for non x86_64 targets"
            exit 1
        fi
    fi;
    srcdir=$(cd >/dev/null $(dirname $0) && pwd -P)
    cat <<EOF
cflags=$cflags -O $(pkg-config --cflags $pkgs)
lflags=$libs
srcdir=$srcdir
buildtype=$buildtype
builddir=$builddir
lablglcflags=$lablglcflags
mudir=$mudir
EOF
    test -e $mudir/build/$buildtype/libmujs.a && echo 'mujs=-lmujs'
    test $native && {
        echo "cmo=.cmx"
        echo "cma=.cmxa"
        command 2>&1 >/dev/null -v ocamlopt.opt && optsuf=".opt" || optsuf=""
        echo "ocamlc=ocamlopt$optsuf"
        echo "linksocclib=-cclib"
        echo "customflag="
    } || {
        echo "cmo=.cmo"
        echo "cma=.cma"
        command 2>&1 >/dev/null -v ocamlc.opt && optsuf=".opt" || optsuf=""
        echo "ocamlc=ocamlc$optsuf"
        echo "linksocclib="
        echo "customflag=-custom"
    }
) >.config || true

cat <<EOF
Configuration results are saved in $(pwd -P)/.config
To build - type: ninja$helpcmdl
EOF
