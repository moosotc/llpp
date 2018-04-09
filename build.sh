#!/bin/sh
set -eu

now() { date +$dfmt; }
unameN=$(uname)
test "$unameN" = Darwin && {
    darwin=true
    wsi="wsi/osx"
} || {
    darwin=false
    wsi="wsi/x11"
}

dfmt="%s"
if $(expr >/dev/null "$(date --version 2>/dev/null)" : ".*GNU"); then
    dfmt="%s.%N"
fi

tstart=$(now)
vecho=${vecho-:}
command -v md5sum >/dev/null || true && alias sum=md5sum
digest() { sum "$@" 2>/dev/null | while read h _; do  printf $h; done; }

partmsg() {
    test $? -eq 0 && msg="ok" || msg="ko"
    echo "$msg $(echo "scale=3; ($(now) - $tstart)/1" | bc -l) sec"
}

die() {
    echo "$*" >&2
    exit 111
}

trap 'partmsg' EXIT

test -n "$1" || die "usage: $0 build-directory"

outd=$1
srcd=$(dirname $0)
mudir=$srcd/mupdf
muinc="-I $mudir/include -I $mudir/thirdparty/freetype/include"

isfresh() {
    test -e "$1" && test -r "$1.past" && {
            . "$1.past"
            test "$k" = "$2"
        }
}

oflags() {
    case "${1#$outd/}" in
        main.cmo|utils.cmo|config.cmo|parser.cmo|wsi.cmi|$wsi/wsi.cmo)
            f="-g -strict-sequence -strict-formats -warn-error a";;
        *) f="-g";;
    esac
    echo "-I lablGL -I $outd/lablGL -I $wsi -I $outd/$wsi -I $outd $f"
}

cflags() {
    case "${1#$outd/}" in
        link.o)
            f="-g -std=c99 -O2 $muinc -Wall -Werror -pedantic-errors"
            $darwin && echo "$f -D__COCOA__" || echo $f;;
        */ml_gl.o) echo "-g -Wno-pointer-sign -O2";;
        *) echo "-g -O2";;
    esac
}

mflags() { echo "-I $(ocamlc -where) -g -O2"; }

bocaml1() {
    eval ocamlc -depend -bytecode -one-line $incs $s | {
        read _ _ depl
        for d in $(eval echo $depl); do
            bocaml ${d#$srcd/} $((n+1))
        done
    }
    cmd="ocamlc $(oflags $o) -c -o $o $s"
    keycmd="digest $o $s"
    grep -q "$o" $outd/ordered || {
        echo "$o" >>$outd/ordered
        isfresh "$o" "$cmd$(eval $keycmd)" || {
            printf "%*.s%s -> %s\n" $n '' "${s#$srcd/}" "$o"
            eval "$cmd"
            echo "k='$cmd$(eval $keycmd)'" >$o.past
        } && $vecho "fresh '$o'"
    }
}

bocaml() (
    o="$1"
    n="$2"
    wocmi="${o%.cmi}"
    test ${wocmi%help.cmo} !=  $wocmi && {
        s=$outd/help.ml
        o=$outd/help.cmo
    } || {
        test "$o" = "$wocmi" && s=$srcd/${o%.cmo}.ml || s=$srcd/$wocmi.mli
        o=$outd/$o
    }
    incs="-I lablGL -I $outd/lablGL -I $wsi -I $outd/$wsi -I $outd"
    bocaml1 "$s" "$o"
)

bocamlc() {
    o=$outd/$1
    s=$srcd/${1%.o}.c
    cmd="ocamlc -ccopt \"$(cflags $o) -MMD -MF $o.dep -MT_ -o $o\" $s"
    test -r $o.dep && read _ d <$o.dep || d=
    keycmd='digest $o $d'
    isfresh "$o" "$cmd$(eval $keycmd)" || {
        printf "%s -> %s\n" "${s#$srcd/}" "$o"
        eval "$cmd"
        read _ d <$o.dep
        echo "k='$cmd$(eval $keycmd)'" >$o.past
    }
}

bobjc() {
    o=$outd/$1
    s=$srcd/${1%.o}.m
    cmd="$mcomp $(mflags $o) -MMD -MF $o.dep -MT_ -c -o $o $s"
    test -r $o.dep && read _ d <$o.dep || d=
    keycmd='digest $o $d'
    isfresh "$o" "$cmd$(eval $keycmd)" || {
        printf "%s -> %s\n" "${s#$srcd/}" "$o"
        eval "$cmd"
        read _ d <$o.dep
        echo "k='$cmd$(eval $keycmd)'" >$o.past
    }
}

mkdir -p $outd/$wsi
mkdir -p $outd/lablGL
:>$outd/ordered

mkhelp() {
    ocaml str.cma -stdin $srcd/KEYS <<EOF
let fixup = let open Str in
  let dash = regexp {|\([^ ]*\) +- +\(.*\)|}
  and head = regexp {|-----\(.*\)-----|} in fun s ->
  String.escaped s |> global_replace dash {|\1\t\2|}
                   |> global_replace head {|\xc2\xb7\1|};;
let rec iter ic = match input_line ic with
| s -> Printf.printf "\"%s\";\\n" @@ fixup s; iter ic
| exception End_of_file -> ();;
Printf.printf "let keys = [\\n";
iter @@ open_in Sys.argv.(1);;
Printf.printf "] and version = \"$ver\";;"
EOF
}

ver=$(cd $srcd && git describe --tags --dirty) || echo unknown
cmd="mkhelp >$outd/help.ml"
keycmd="digest $srcd/KEYS; echo $ver"
isfresh "$outd/help.ml" '$cmd$(eval $keycmd)$ver' || {
    eval $cmd
    echo "k='$cmd$(eval $keycmd)$ver'" >$outd/help.ml.past
}

# following is disgusting (from "generalize everything" perspective),
# but generic method of derviving .ml's location from .mli's is not
# immediately obvious
for m in lablGL/glMisc.cmo lablGL/glTex.cmo $wsi/wsi.cmo main.cmo; do
    bocaml $m 0
done
bocamlc link.o
cobjs="$outd/link.o"

libs="str.cma unix.cma"
clibs="-L$mudir/build/native -lmupdf -lmupdfthird -lpthread"
if $darwin; then
    mcomp=$(ocamlc -config | grep bytecomp_c_co | { read _ c; echo $c; })
    clibs="$clibs -framework Cocoa -framework OpenGL"
    bobjc main_osx.o
    cobjs="$cobjs $outd/main_osx.o"
else
    clibs="$clibs -lGL -lX11"
fi

globjs=
for f in ml_gl ml_glarray ml_raw; do
    bocamlc lablGL/$f.o
    globjs="$globjs $outd/lablGL/$f.o"
done

ord=$(echo $(grep -v \.cmi $outd/ordered))
cmd="ocamlc -custom $libs -o $outd/llpp $cobjs $ord"
cmd="$cmd $globjs -cclib \"$clibs\""
keycmd="digest $outd/llpp $outd/link.o $ord"
isfresh "$outd/llpp" "$cmd$(eval $keycmd)" || {
        echo linking $outd/llpp
        eval $cmd || echo "$cmd failed"
        echo "k='$cmd$(eval $keycmd)'" >$outd/llpp.past
    }

if $darwin; then
    shortver=$(echo $ver | sed -n 's/v\([0-9]*\).*/\1/p')
    mkdir -p $outd/llpp.app/Contents/MacOS
    cat misc/Info.plist | sed s/@VERSION@/$shortver/ | sed s/@BUNDLE_VERSION@/$ver/ > $outd/llpp.app/Contents/Info.plist
    cp $outd/llpp $outd/llpp.app/Contents/MacOS/
fi
