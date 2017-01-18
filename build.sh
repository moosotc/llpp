#!/bin/sh
macos=$(test $(uname -s) = "Darwin" && echo 1 || echo 0)
set -e
if test x"$1" = x; then
    printf "No build directory specified\n" 1>&2
    exit 1
else
    builddir="$1"
fi
test $(ocamlc --version | awk -F. '{print $1 $2}') -lt 404 && {
    echo "OCaml version 4.04.0 or higher required"
    exit 1
} || true

test x"$2" = "x" || cty="$2" && cty=""

ccopt="$CFLAGS -Wno-pointer-sign -O2"
mlopt='-warn-error +a -w +a -g -safe-string'
if test -z "$native"; then
    comp=ocamlc$cty
    osu=.cmo
    asu=.cma
    lfl=-custom
else
    comp=ocamlopt$cty
    osu=.cmx
    asu=.cmxa
    lfl=
fi
mkdir -p "$builddir/lablGL"
srcdir=$(dirname $0)
version=$(cd $srcdir && git describe --tags 2>/dev/null) || version=unknown
mloptgl="-I $srcdir/lablGL -I $builddir/lablGL"
set -x
$comp -ccopt "$ccopt -o $builddir/lablGL/ml_raw.o" -c $srcdir/lablGL/ml_raw.c
$comp -ccopt "$ccopt -o $builddir/lablGL/ml_gl.o" -c $srcdir/lablGL/ml_gl.c
$comp -ccopt "$ccopt -o $builddir/lablGL/ml_glarray.o" -c $srcdir/lablGL/ml_glarray.c
if test $macos -eq 1; then # On Mac OS X platform
    $comp -ccopt "-I $srcdir/mupdf/include -I $srcdir/mupdf/thirdparty/freetype/include -I /usr/X11/include -Wextra -Wall -Werror -D_GNU_SOURCE -O -g -std=c99 -pedantic-errors -Wunused-parameter -Wsign-compare -Wshadow -o $builddir/link.o" -c $srcdir/link.c
else
    $comp -ccopt "-I $srcdir/mupdf/include -I $srcdir/mupdf/thirdparty/freetype/include -Wextra -Wall -Werror -D_GNU_SOURCE -O -g -std=c99 -pedantic-errors -Wunused-parameter -Wsign-compare -Wshadow -o $builddir/link.o" -c $srcdir/link.c
fi
/bin/sh $srcdir/mkhelp.sh $srcdir/KEYS "$version" >$builddir/help.ml
$comp -c $mloptgl -o $builddir/keys$osu $srcdir/keys.ml
$comp -c $mloptgl -o $builddir/lablGL/gl$osu $srcdir/lablGL/gl.ml
$comp -c $mloptgl -o $builddir/lablGL/raw$osu $srcdir/lablGL/raw.ml
$comp -c $mloptgl -o $builddir/lablGL/glPix$osu $srcdir/lablGL/glPix.ml
$comp -c $mloptgl -o $builddir/lablGL/glDraw$osu $srcdir/lablGL/glDraw.ml
$comp -c $mloptgl -o $builddir/lablGL/glTex.cmi $srcdir/lablGL/glTex.mli
$comp -c $mloptgl -o $builddir/lablGL/glMisc.cmi $srcdir/lablGL/glMisc.mli
$comp -c $mloptgl -o $builddir/lablGL/glMat$osu $srcdir/lablGL/glMat.ml
$comp -c $mloptgl -o $builddir/lablGL/glMisc$osu $srcdir/lablGL/glMisc.ml
$comp -c $mloptgl -o $builddir/lablGL/glFunc$osu $srcdir/lablGL/glFunc.ml
$comp -c $mloptgl -o $builddir/lablGL/glTex$osu $srcdir/lablGL/glTex.ml
$comp -c $mloptgl -o $builddir/lablGL/glArray$osu $srcdir/lablGL/glArray.ml
$comp -c $mloptgl -o $builddir/lablGL/glClear$osu $srcdir/lablGL/glClear.ml
$comp -c -o $builddir/help$osu $builddir/help.ml
$comp -c $mlopt -o $builddir/utils$osu $srcdir/utils.ml
$comp -c $mlopt -I $builddir -o $builddir/parser$osu $srcdir/parser.ml
$comp -c $mlopt -I $builddir -o $builddir/wsi.cmi $srcdir/wsi.mli
$comp -c $mloptgl -I $builddir -o $builddir/config$osu $srcdir/config.ml
$comp -c $mloptgl -I $builddir -o $builddir/main$osu $srcdir/main.ml
$comp -c $mlopt -I $builddir -o $builddir/wsi$osu $srcdir/wsi.ml
if test $macos -eq 1; then # on Mac OS X platform
    $comp -g $lfl -I lablGL -o $builddir/llpp unix$asu str$asu $builddir/help$osu $builddir/lablGL/raw$osu $builddir/utils$osu $builddir/parser$osu $builddir/lablGL/glMisc$osu $builddir/wsi$osu $builddir/lablGL/gl$osu $builddir/lablGL/glMat$osu $builddir/lablGL/glFunc$osu $builddir/lablGL/glClear$osu $builddir/lablGL/glPix$osu $builddir/lablGL/glTex$osu $builddir/lablGL/glDraw$osu $builddir/config$osu $builddir/lablGL/glArray$osu $builddir/main$osu $builddir/link.o -cclib "-L/usr/X11/lib -lGL -L/usr/X11 -lX11 -lmupdf -lmupdfthird -lpthread -L$srcdir/mupdf/build/native -lcrypto $builddir/lablGL/ml_gl.o $builddir/lablGL/ml_glarray.o $builddir/lablGL/ml_raw.o"
else
    $comp -g $lfl -I lablGL -o $builddir/llpp unix$asu str$asu $builddir/help$osu $builddir/lablGL/raw$osu $builddir/utils$osu $builddir/parser$osu $builddir/lablGL/glMisc$osu $builddir/wsi$osu $builddir/lablGL/gl$osu $builddir/lablGL/glMat$osu $builddir/lablGL/glFunc$osu $builddir/lablGL/glClear$osu $builddir/lablGL/glPix$osu $builddir/lablGL/glTex$osu $builddir/lablGL/glDraw$osu $builddir/config$osu $builddir/lablGL/glArray$osu $builddir/main$osu $builddir/link.o -cclib "-lGL -lX11 -lmupdf -lmupdfthird -lpthread -L$srcdir/mupdf/build/native -lcrypto $builddir/lablGL/ml_gl.o $builddir/lablGL/ml_glarray.o $builddir/lablGL/ml_raw.o"
fi
