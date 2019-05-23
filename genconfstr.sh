#!/bin/sh
set -eu

cat<<EOF
open Utils;;
type rgb = float * float * float
type rgba = float * float * float * float
type fitmodel = | FitWidth | FitProportional | FitPage
type irect = (int * int * int * int)
and bbox = irect
type colorspace = | Rgb | Gray
type keymap =
  | KMinsrt of key | KMinsrl of key list | KMmulti of key list * key list
and key = int * int
and keyhash = (key, keymap) Hashtbl.t
and keystate = |KSnone |KSinto of (key list * key list)
and css = string
type columns =
  | Csingle of singlecolumn
  | Cmulti of multicolumns
  | Csplit of splitcolumns
and mark =
  | Mark_page
  | Mark_block
  | Mark_line
  | Mark_word
and multicolumns = multicol * pagegeom
and singlecolumn = pagegeom
and splitcolumns = columncount * pagegeom
and pagegeom = (pdimno * x * y * (pageno * width * height * leftx)) array
and multicol = columncount * covercount * covercount
and columncount = int
and pdimno = int
and pageno = int
and x = int and y = int and leftx = int
and covercount = int
and width = int and height = int
and memsize = int and texcount = int
and sliceheight = int;;
let scrollbvv = 1 and scrollbhv = 2;;
EOF

init=
assi=
g() {
    printf "mutable $1:$2;"
    init="$init $1=$3;"
    assi="$assi dst.$1 <- src.$1;"
}
i() { g "$1" int "$2"; }
b() { g "$1" bool "$2"; }
f() { g "$1" float "$2"; }
s() { g "$1" string "$2"; }
K() {
    printf "mutable $1:$2;\n"
    init="$init $1=$3;"
    assi="$assi dst.keyhashes <- copykeyhashes src;"
}
P() {
    printf "mutable $1 : float option;\n"
    init="$init $1=None;"
    assi="$assi dst.pax <- if src.pax = None then None else Some 0.0;"
}
echo "type conf = {"
i scrollbw 7
i scrollh 12
i scrollb "scrollbhv lor scrollbvv"
b icase true
b preload true
i pagebias 0
b verbose false
b debug false
i scrollstep 24
i hscrollstep 24
b maxhfit true
i autoscrollstep 2
b hlinks false
b underinfo false
i interpagespace 2
f zoom 1.0
b presentation false
i angle 0
i cwinw 1200
i cwinh 1000
g fitmodel fitmodel FitProportional
b trimmargins false
g trimfuzz irect "(0,0,0,0)"
g memlimit memsize "32 lsl 20"
g texcount texcount 256
g sliceheight sliceheight 24
g thumbw width 76
g bgcolor rgb "(0.5, 0.5, 0.5)"
g papercolor rgba "(1.0, 1.0, 1.0, 0.0)"
g sbarcolor rgba "(0.64, 0.64, 0.64, 0.7)"
g sbarhndlcolor rgba "(0.0, 0.0, 0.0, 0.7)"
g texturecolor rgba "(0.0, 0.0, 0.0, 0.0)"
b bedefault false
i tilew 2048
i tileh 2048
g mustoresize memsize "256 lsl 20"
b checkers true
i aalevel 8
s urilauncher '(match[@warning "-4"] platform with |Pmacos -> {|open "%s"|}|_ -> {|echo "%s"|})'
s pathlauncher '{|lp "%s"|}'
g colorspace colorspace Rgb
b invert false
f colorscale 1.
g columns columns "Csingle [||]"
g beyecolumns "columncount option" None
s selcmd '(match platform with |Plinux|Pbsd -> "LC_CTYPE=UTF-8 xclip -i"|Pmacos -> "LC_CTYPE=UTF-8 pbcopy"|Punknown -> "cat")'
s pastecmd '(match platform with |Plinux|Pbsd -> "LC_CTYPE=UTF-8 xclip -o"|Pmacos -> "LC_CTYPE=UTF-8 pbpaste"|Punknown -> "echo")'
s paxcmd '"cat"'
s passcmd E.s
s savecmd E.s
b updatecurs true
K keyhashes '(string * keyhash) list' \
'(let mk n = (n, Hashtbl.create 1) in
      [ mk "global"; mk "info" ; mk "help"; mk "outline"; mk "listview"
      ; mk "birdseye"; mk "textentry"; mk "links"; mk "view" ])'
i hfsize '12 * Wsi.fontsizefactor ()'
f pgscale 1.
b usepbo false
b wheelbypage false
s stcmd "{|echo SyncTex|}"
b riani false
g paxmark mark Mark_word
b leftscroll false
s title E.s
f lastvisit 0.0
b annotinline true
b coarseprespos false
g css css E.s
b usedoccss true
s key E.s
P pax

cat <<EOF
};;
let copykeyhashes c = List.map (fun (k, v) -> k, Hashtbl.copy v) c.keyhashes;;
let defconf = {$init};;
let setconf dst src = $assi;
EOF
