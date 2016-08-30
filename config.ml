open Utils;;

external fz_version : unit -> string = "ml_fz_version";;

type fontstate =
    { mutable fontsize : int
    ; mutable wwidth : float
    ; mutable maxrows : int
    }
;;

let fstate =
  { fontsize = 14
  ; wwidth = nan
  ; maxrows = -1
  }
;;

let scrollbvv = 1;;
let scrollbhv = 2;;
let fastghyllscroll = (5,1,2);;
let neatghyllscroll = (10,1,9);;

let irect_of_string s =
  Scanf.sscanf s "%d/%d/%d/%d" (fun x0 y0 x1 y1 -> (x0,y0,x1,y1))
;;

let irect_to_string (x0,y0,x1,y1) =
  Printf.sprintf "%d/%d/%d/%d" x0 y0 x1 y1
;;

let ghyllscroll_of_string s =
  match s with
  | "fast" -> Some fastghyllscroll
  | "neat" -> Some (10,1,9)
  | "" | "none" -> None
  | _ ->
      let (n,a,b) as nab =
        Scanf.sscanf s "%u,%u,%u" (fun n a b -> n, a, b) in
      if n <= a || n <= b || a >= b
      then error "N(%d),A(%d),B(%d) (N <= A, A < B, N <= B)" n a b;
      Some nab
;;

let ghyllscroll_to_string ((n, a, b) as nab) =
  (**) if nab = fastghyllscroll then "fast"
  else if nab = neatghyllscroll then "neat"
  else Printf.sprintf "%d,%d,%d" n a b;
;;

let multicolumns_to_string (n, a, b) =
  if a = 0 && b = 0
  then Printf.sprintf "%d" n
  else Printf.sprintf "%d,%d,%d" n a b;
;;

let multicolumns_of_string s =
  try
    (int_of_string s, 0, 0)
  with _ ->
    Scanf.sscanf s "%u,%u,%u" (fun n a b ->
      if a > 1 || b > 1
      then failwith "subtly broken";
      (n, a, b)
    );
;;

type keymap        =
    | KMinsrt of key
    | KMinsrl of key list
    | KMmulti of key list * key list
and key            = int * int
and keyhash        = (key, keymap) Hashtbl.t
and keystate       =
    | KSnone
    | KSinto of (key list * key list)
and interpagespace = int
and multicolumns   = multicol * pagegeom
and singlecolumn   = pagegeom
and splitcolumns   = columncount * pagegeom
and pagegeom       = (pdimno * int * int * (pageno * width * height * leftx)) array
and multicol       = columncount * covercount * covercount
and pdimno         = int
and columncount    = int
and covercount     = int
and fitmodel       = | FitWidth | FitProportional | FitPage
and trimmargins    = bool
and irect          = (int * int * int * int)
and memsize        = int
and texcount       = int
and sliceheight    = int
and angle          = int
and params         = (angle * fitmodel * trimparams
                         * texcount * sliceheight * memsize
                         * colorspace * fontpath * trimcachepath
                         * haspbo * usefontconfig)
and width          = int
and height         = int
and leftx          = int
and opaque         = Opaque.t
and rectcolor      = (float * float * float * float)
and pixmapsize     = int
and gen            = int
and top            = float
and dtop           = float
and fontpath       = string
and trimcachepath  = string
and aalevel        = int
and trimparams     = (trimmargins * irect)
and colorspace     = | Rgb | Bgr | Gray
and haspbo         = bool
and usefontconfig  = bool
and uri            = string
and caption        = string
and x              = float
and y              = float
and tilex          = int
and tiley          = int
and tileparams     = (int * int * width * height * tilex * tiley)
and under =
    | Unone
    | Ulinkuri of string
    | Ulinkgoto of (int * int)
    | Utext of facename
    | Uunexpected of string
    | Ulaunch of launchcommand
    | Unamed of destname
    | Uremote of (filename * pageno)
    | Uremotedest of (filename * destname)
    | Uannotation of (opaque * slinkindex)
and slinkindex = int
and facename = string
and launchcommand = string
and filename = string
and pageno = int
and destname = string
and mark =
    | Mark_page
    | Mark_block
    | Mark_line
    | Mark_word
and link =
    | Lnotfound
    | Lfound of int
and linkdir =
    | LDfirst
    | LDlast
    | LDfirstvisible of (int * int * int)
    | LDleft of int
    | LDright of int
    | LDdown of int
    | LDup of int
and pagewithlinks =
    | Pwlnotfound
    | Pwl of int
and scrollb = int
and anchor = pageno * top * dtop
and rect = float * float * float * float * float * float * float * float
and infochange = | Memused | Docinfo | Pdim
;;

class type uioh = object
  method display : unit
  method key : int -> int -> uioh
  method button : int -> bool -> int -> int -> int -> uioh
  method multiclick : int -> int -> int -> int -> uioh
  method motion : int -> int -> uioh
  method pmotion : int -> int -> uioh
  method infochanged : infochange -> unit
  method scrollpw : (int * float * float)
  method scrollph : (int * float * float)
  method modehash : keyhash
  method eformsgs : bool
  method alwaysscrolly : bool
end;;

module type TextEnumType =
sig
  type t
  val name : string
  val names : string array
end;;

module TextEnumMake (Ten : TextEnumType) =
struct
  let names = Ten.names;;
  let to_int (t : Ten.t)  = Obj.magic t;;
  let to_string t = names.(to_int t);;
  let of_int n : Ten.t = Obj.magic n;;
  let of_string s =
    let rec find i =
      if i = Array.length names
      then failwith ("invalid " ^ Ten.name ^ ": " ^ s)
      else (
        if Ten.names.(i) = s
        then of_int i
        else find (i+1)
      )
    in find 0;;
end;;

module CSTE = TextEnumMake (struct
  type t = colorspace;;
  let name = "colorspace";;
  let names = [|"rgb"; "bgr"; "gray"|];;
end);;

module MTE = TextEnumMake (struct
  type t = mark;;
  let name = "mark";;
  let names = [|"page"; "block"; "line"; "word"|];;
end);;

module FMTE = TextEnumMake (struct
  type t = fitmodel;;
  let name = "fitmodel";;
  let names = [|"width"; "proportional"; "page"|];;
end);;

type conf =
    { mutable scrollbw       : int
    ; mutable scrollh        : int
    ; mutable scrollb        : scrollb
    ; mutable icase          : bool
    ; mutable preload        : bool
    ; mutable pagebias       : int
    ; mutable verbose        : bool
    ; mutable debug          : bool
    ; mutable scrollstep     : int
    ; mutable hscrollstep    : int
    ; mutable maxhfit        : bool
    ; mutable crophack       : bool
    ; mutable autoscrollstep : int
    ; mutable maxwait        : float option
    ; mutable hlinks         : bool
    ; mutable underinfo      : bool
    ; mutable interpagespace : interpagespace
    ; mutable zoom           : float
    ; mutable presentation   : bool
    ; mutable angle          : angle
    ; mutable cwinw          : int
    ; mutable cwinh          : int
    ; mutable savebmarks     : bool
    ; mutable fitmodel       : fitmodel
    ; mutable trimmargins    : trimmargins
    ; mutable trimfuzz       : irect
    ; mutable memlimit       : memsize
    ; mutable texcount       : texcount
    ; mutable sliceheight    : sliceheight
    ; mutable thumbw         : width
    ; mutable jumpback       : bool
    ; mutable bgcolor        : (float * float * float)
    ; mutable bedefault      : bool
    ; mutable tilew          : int
    ; mutable tileh          : int
    ; mutable mustoresize    : memsize
    ; mutable checkers       : bool
    ; mutable aalevel        : int
    ; mutable urilauncher    : string
    ; mutable pathlauncher   : string
    ; mutable colorspace     : colorspace
    ; mutable invert         : bool
    ; mutable colorscale     : float
    ; mutable ghyllscroll    : (int * int * int) option
    ; mutable columns        : columns
    ; mutable beyecolumns    : columncount option
    ; mutable selcmd         : string
    ; mutable paxcmd         : string
    ; mutable passcmd        : string
    ; mutable savecmd        : string
    ; mutable updatecurs     : bool
    ; mutable keyhashes      : (string * keyhash) list
    ; mutable hfsize         : int
    ; mutable pgscale        : float
    ; mutable usepbo         : bool
    ; mutable wheelbypage    : bool
    ; mutable stcmd          : string
    ; mutable riani          : bool
    ; mutable pax            : (float * int * int) ref option
    ; mutable paxmark        : mark
    ; mutable leftscroll     : bool
    ; mutable title          : string
    ; mutable lastvisit      : float
    ; mutable annotinline    : bool
    ; mutable coarseprespos  : bool
    }
and columns =
    | Csingle of singlecolumn
    | Cmulti of multicolumns
    | Csplit of splitcolumns
and outlinekind =
    | Onone
    | Oanchor of anchor
    | Ouri of uri
    | Olaunch of launchcommand
    | Oremote of (filename * pageno)
    | Oremotedest of (filename * destname)
    | Ohistory of (filename * conf * outline list * x * anchor * filename)
and outline = (caption * outlinelevel * outlinekind)
and outlinelevel = int
;;

type page =
    { pageno    : int
    ; pagedimno : int
    ; pagew     : int
    ; pageh     : int
    ; pagex     : int
    ; pagey     : int
    ; pagevw    : int
    ; pagevh    : int
    ; pagedispx : int
    ; pagedispy : int
    ; pagecol   : int
    }
;;

type tile = opaque * pixmapsize * elapsed
and elapsed = float;;
type pagemapkey = pageno * gen;;
type tilemapkey = pageno * gen * colorspace * angle * width * height * col * row
and row = int
and col = int
and currently =
  | Idle
  | Loading of (page * gen)
  | Tiling of (
      page * opaque * colorspace * angle * gen * col * row * width * height
     )
  | Outlining of outline list
;;

type mpos = int * int
and mstate =
    | Msel of (mpos * mpos)
    | Mpan of mpos
    | Mscrolly | Mscrollx
    | Mzoom of (buttonno * step * mpos)
    | Mzoomrect of (mpos * mpos)
    | Mnone
and buttonno = int
and step = int
;;

type mode =
    | Birdseye of (conf * leftx * pageno * pageno * anchor)
    | Textentry of (textentry * onleave)
    | View
    | LinkNav of linktarget
and onleave = leavetextentrystatus -> unit
and leavetextentrystatus = | Cancel | Confirm
and helpitem = string * int * action
and action =
    | Noaction
    | Action of (uioh -> uioh)
and linktarget =
    | Ltexact of (pageno * direction)
    | Ltgendir of direction
    | Ltnotready of (pageno * direction)
and direction = int             (* -1, 0, 1 *)
and textentry = string * string * onhist option * onkey * ondone * cancelonempty
and onkey = string -> int -> te
and ondone = string -> unit
and histcancel = unit -> unit
and onhist = ((histcmd -> string) * histcancel)
and histcmd = HCnext | HCprev | HCfirst | HClast
and cancelonempty = bool
and te =
    | TEstop
    | TEdone of string
    | TEcont of string
    | TEswitch of textentry
;;

type 'a circbuf =
    { store : 'a array
    ; mutable rc : int
    ; mutable wc : int
    ; mutable len : int
    }
;;

type state =
    { mutable ss            : Unix.file_descr
    ; mutable wsfd          : Unix.file_descr
    ; mutable stderr        : Unix.file_descr
    ; mutable errmsgs       : Buffer.t
    ; mutable newerrmsgs    : bool
    ; mutable w             : int
    ; mutable x             : x
    ; mutable y             : y
    ; mutable anchor        : anchor
    ; mutable ranchors      : (string * string * anchor * string) list
    ; mutable maxy          : y
    ; mutable layout        : page list
    ; pagemap               : (pagemapkey, opaque) Hashtbl.t
    ; tilemap               : (tilemapkey, tile) Hashtbl.t
    ; tilelru               : (tilemapkey * opaque * pixmapsize) Queue.t
    ; mutable pdims         : (pageno * width * height * leftx) list
    ; mutable pagecount     : int
    ; mutable currently     : currently
    ; mutable mstate        : mstate
    ; mutable searchpattern : string
    ; mutable rects         : (pageno * rectcolor * rect) list
    ; mutable rects1        : (pageno * rectcolor * rect) list
    ; prects                : (pageno, float array) Hashtbl.t
    ; mutable text          : string
    ; mutable winstate      : Wsi.winstate list
    ; mutable mode          : mode
    ; mutable uioh          : uioh
    ; mutable outlines      : outline array
    ; mutable bookmarks     : outline list
    ; mutable path          : string
    ; mutable password      : string
    ; mutable nameddest     : string
    ; mutable geomcmds      : (string * ((string * (unit -> unit)) list))
    ; mutable memused       : memsize
    ; mutable gen           : gen
    ; mutable throttle      : (page list * float * float) option
    ; mutable autoscroll    : int option
    ; mutable ghyll         : (int option -> unit)
    ; mutable help          : helpitem array
    ; mutable docinfo       : (int * string) list
    ; mutable checkerstexid : GlTex.texture_id option
    ; hists                 : hists
    ; mutable prevzoom      : (float * int)
    ; mutable progress      : float
    ; mutable redisplay     : bool
    ; mutable mpos          : mpos
    ; mutable keystate      : keystate
    ; mutable glinks        : bool
    ; mutable prevcolumns   : (columns * float) option
    ; mutable winw          : int
    ; mutable winh          : int
    ; mutable reprf         : (unit -> unit)
    ; mutable origin        : string
    ; mutable roam          : (unit -> unit)
    ; mutable bzoom         : bool
    ; mutable traw          : [`float] Raw.t
    ; mutable vraw          : [`float] Raw.t
    }
and hists =
    { pat : string circbuf
    ; pag : string circbuf
    ; nav : anchor circbuf
    ; sel : string circbuf
    }
;;

let emptyanchor = (0, 0.0, 0.0);;
let emptykeyhash = Hashtbl.create 0;;
let noghyll _ = ();;
let noreprf () = ();;
let noroam () = ();;

let nouioh : uioh = object (self)
  method display = ()
  method key _ _ = self
  method multiclick _ _ _ _ = self
  method button _ _ _ _ _ = self
  method motion _ _ = self
  method pmotion _ _ = self
  method infochanged _ = ()
  method scrollpw = (0, nan, nan)
  method scrollph = (0, nan, nan)
  method modehash = emptykeyhash
  method eformsgs = false
  method alwaysscrolly = false
end;;

let platform_to_string = function
  | Punknown      -> "unknown"
  | Plinux        -> "Linux"
  | Posx          -> "OSX"
  | Psun          -> "Sun"
  | Pbsd          -> "BSD"
  | Pcygwin       -> "Cygwin"
;;

let version () =
  Printf.sprintf "llpp version %s, fitz %s, ocaml %s/%d bit"
    Help.version (fz_version ()) Sys.ocaml_version Sys.word_size
;;

let defconf =
  { scrollbw       = 7
  ; scrollh        = 12
  ; scrollb        = scrollbhv lor scrollbvv
  ; icase          = true
  ; preload        = true
  ; pagebias       = 0
  ; verbose        = false
  ; debug          = false
  ; scrollstep     = 24
  ; hscrollstep    = 24
  ; maxhfit        = true
  ; crophack       = false
  ; autoscrollstep = 2
  ; maxwait        = None
  ; hlinks         = false
  ; underinfo      = false
  ; interpagespace = 2
  ; zoom           = 1.0
  ; presentation   = false
  ; angle          = 0
  ; cwinw          = 900
  ; cwinh          = 900
  ; savebmarks     = true
  ; fitmodel       = FitProportional
  ; trimmargins    = false
  ; trimfuzz       = (0,0,0,0)
  ; memlimit       = 32 lsl 20
  ; texcount       = 256
  ; sliceheight    = 24
  ; thumbw         = 76
  ; jumpback       = true
  ; bgcolor        = (0.5, 0.5, 0.5)
  ; bedefault      = false
  ; tilew          = 2048
  ; tileh          = 2048
  ; mustoresize    = 256 lsl 20
  ; checkers       = true
  ; aalevel        = 8
  ; urilauncher    =
      (match platform with
      | Plinux | Psun | Pbsd -> "xdg-open \"%s\""
      | Posx -> "open \"%s\""
      | Pcygwin -> "cygstart \"%s\""
      | Punknown -> "echo %s")
  ; pathlauncher   = "lp \"%s\""
  ; selcmd         =
      (match platform with
      | Plinux | Pbsd | Psun -> "xsel -i"
      | Posx -> "pbcopy"
      | Pcygwin -> "wsel"
      | Punknown -> "cat")
  ; paxcmd         = "cat"
  ; passcmd        = E.s
  ; savecmd        = E.s
  ; colorspace     = Rgb
  ; invert         = false
  ; colorscale     = 1.0
  ; ghyllscroll    = None
  ; columns        = Csingle [||]
  ; beyecolumns    = None
  ; updatecurs     = false
  ; hfsize         = 12
  ; pgscale        = 1.0
  ; usepbo         = false
  ; wheelbypage    = false
  ; stcmd          = "echo SyncTex"
  ; riani          = false
  ; pax            = None
  ; paxmark        = Mark_word
  ; leftscroll     = false
  ; title          = E.s
  ; lastvisit      = 0.0
  ; annotinline    = true
  ; coarseprespos  = false
  ; keyhashes      =
      let mk n = (n, Hashtbl.create 1) in
      [ mk "global"
      ; mk "info"
      ; mk "help"
      ; mk "outline"
      ; mk "listview"
      ; mk "birdseye"
      ; mk "textentry"
      ; mk "links"
      ; mk "view"
      ]
  }
;;

let conf = { defconf with angle = defconf.angle };;

let gotourl url =
  let command = Str.global_replace percentsre url conf.urilauncher in
  try ignore @@ spawn command []
  with exn -> dolog "failed to execute `%s': %s" command @@ exntos exn
;;

let gotouri uri =
  if emptystr conf.urilauncher
  then dolog "%s" uri
  else (
    let url = geturl uri in
    if emptystr url
    then dolog "obtained empty url from uri %S" uri
    else gotourl url
  );
;;

let makehelp () =
  let strings =
    version ()
    :: "(searching in this text works just by typing (i.e. no initial '/'))"
    :: E.s :: Help.keys
  in
  Array.of_list (
    List.map (fun s ->
      let url = geturl s in
      if nonemptystr url
      then (s, 0, Action (fun uioh -> gotourl url; uioh))
      else (s, 0, Noaction)
    ) strings);
;;

let cbnew n v =
  { store = Array.make n v
  ; rc = 0
  ; wc = 0
  ; len = 0
  }
;;

let cbcap b = Array.length b.store;;

let cbput b v =
  let cap = cbcap b in
  b.store.(b.wc) <- v;
  b.wc <- (b.wc + 1) mod cap;
  b.rc <- b.wc;
  b.len <- min (b.len + 1) cap;
;;

let cbempty b = b.len = 0;;

let cbgetg b circular dir =
  if cbempty b
  then b.store.(0)
  else
    let rc = b.rc + dir in
    let rc =
      if circular
      then (
        if rc = -1
        then b.len-1
        else (
          if rc >= b.len
          then 0
          else rc
        )
      )
      else bound rc 0 (b.len-1)
    in
    b.rc <- rc;
    b.store.(rc);
;;

let cbget b = cbgetg b false;;
let cbgetc b = cbgetg b true;;

let state =
  { ss            = Unix.stdin
  ; wsfd          = Unix.stdin
  ; stderr        = Unix.stderr
  ; errmsgs       = Buffer.create 0
  ; newerrmsgs    = false
  ; x             = 0.0
  ; y             = 0.0
  ; w             = 0
  ; anchor        = emptyanchor
  ; ranchors      = []
  ; layout        = []
  ; maxy          = max_float
  ; tilelru       = Queue.create ()
  ; pagemap       = Hashtbl.create 10
  ; tilemap       = Hashtbl.create 10
  ; pdims         = []
  ; pagecount     = 0
  ; currently     = Idle
  ; mstate        = Mnone
  ; rects         = []
  ; rects1        = []
  ; prects        = Hashtbl.create 1
  ; text          = E.s
  ; mode          = View
  ; winstate      = []
  ; searchpattern = E.s
  ; outlines      = E.a
  ; bookmarks     = []
  ; path          = E.s
  ; password      = E.s
  ; nameddest     = E.s
  ; geomcmds      = E.s, []
  ; hists         =
      { nav       = cbnew 10 emptyanchor
      ; pat       = cbnew 10 E.s
      ; pag       = cbnew 10 E.s
      ; sel       = cbnew 10 E.s
      }
  ; memused       = 0
  ; gen           = 0
  ; throttle      = None
  ; autoscroll    = None
  ; ghyll         = noghyll
  ; help          = makehelp ()
  ; docinfo       = []
  ; checkerstexid = None
  ; prevzoom      = (1.0, 0)
  ; progress      = -1.0
  ; uioh          = nouioh
  ; redisplay     = true
  ; mpos          = (-1, -1)
  ; keystate      = KSnone
  ; glinks        = false
  ; prevcolumns   = None
  ; winw          = -1
  ; winh          = -1
  ; reprf         = noreprf
  ; origin        = E.s
  ; roam          = noroam
  ; bzoom         = false
  ; traw          = Raw.create_static `float ~len:8
  ; vraw          = Raw.create_static `float ~len:8
  }
;;

let copykeyhashes c =
  List.map (fun (k, v) -> k, Hashtbl.copy v) c.keyhashes;
;;

let calcips h =
  let d = state.winh - h in
  max conf.interpagespace ((d + 1) / 2)
;;

let rowyh (c, coverA, coverB) b n =
  if c = 1 || (n < coverA || n >= state.pagecount - coverB)
  then
    let _, _, vy, (_, _, h, _) = b.(n) in
    (vy, h)
  else
    let n' = n - coverA in
    let d = n' mod c in
    let s = n - d in
    let e = min state.pagecount (s + c) in
    let rec find m miny maxh = if m = e then miny, maxh else
        let _, _, y, (_, _, h, _) = b.(m) in
        let miny = min miny y in
        let maxh = max maxh h in
        find (m+1) miny maxh
    in find s max_int 0
;;

let page_of_y y =
  let ((c, coverA, coverB) as cl), b =
    match conf.columns with
    | Csingle b -> (1, 0, 0), b
    | Cmulti (c, b) -> c, b
    | Csplit (_, b) -> (1, 0, 0), b
  in
  if Array.length b = 0
  then -1
  else
    let rec bsearch nmin nmax =
      if nmin > nmax
      then bound nmin 0 (state.pagecount-1)
      else
        let n = (nmax + nmin) / 2 in
        let vy, h = rowyh cl b n in
        let vy = float vy in
        let y0, y1 =
          if conf.presentation
          then
            let ips = calcips h in
            let y0 = vy -. float ips in
            let y1 = vy +. float (h + ips) in
            y0, y1
          else (
            if n = 0
            then 0.0, vy +. float (h + conf.interpagespace)
            else
              let y0 = vy -. float conf.interpagespace in
              y0, y0 +. float (h + conf.interpagespace)
          )
        in
        if y >= y0 && y < y1
        then (
          if c = 1
          then n
          else (
            if n > coverA
            then
              if n < state.pagecount - coverB
              then ((n-coverA)/c)*c + coverA
              else n
            else n
          )
        )
        else (
          if y > y0
          then bsearch (n+1) nmax
          else bsearch nmin (n-1)
        )
    in
    bsearch 0 (state.pagecount-1);
;;

let calcheight () =
  match conf.columns with
  | Cmulti ((_, _, _) as cl, b) ->
      if Array.length b > 0
      then
        let y, h = rowyh cl b (Array.length b - 1) in
        float (y + h + (if conf.presentation then calcips h else 0))
      else 0.0
  | Csingle b ->
      if Array.length b > 0
      then
        let (_, _, y, (_, _, h, _)) = b.(Array.length b - 1) in
        float (y + h + (if conf.presentation then calcips h else 0))
      else 0.0
  | Csplit (_, b) ->
      if Array.length b > 0
      then
        let (_, _, y, (_, _, h, _)) = b.(Array.length b - 1) in
        float (y + h)
      else 0.0
;;

let getpageywh pageno =
  let pageno = bound pageno 0 (state.pagecount-1) in
  match conf.columns with
  | Csingle b ->
      if Array.length b = 0
      then 0.0, 0, 0
      else
        let (_, _, y, (_, w, h, _)) = b.(pageno) in
        let y =
          float @@
            if conf.presentation
            then y - calcips h
            else y
        in
        y, w, h
  | Cmulti (cl, b) ->
      if Array.length b = 0
      then 0.0, 0, 0
      else
        let y, h = rowyh cl b pageno in
        let (_, _, _, (_, w, _, _)) = b.(pageno) in
        let y =
          float @@
            if conf.presentation
            then y - calcips h
            else y
        in
        y, w, h
  | Csplit (c, b) ->
      if Array.length b = 0
      then 0.0, 0, 0
      else
        let n = pageno*c in
        let (_, _, y, (_, w, h, _)) = b.(n) in
        float y, w / c, h
;;

let getpageyh pageno =
  let y,_,h = getpageywh pageno in
  y, h;
;;

let getpagedim pageno =
  let rec f ppdim l =
    match l with
    | (n, _, _, _) as pdim :: rest ->
        if n >= pageno
        then (if n = pageno then pdim else ppdim)
        else f pdim rest

    | [] -> ppdim
  in
  f (-1, -1, -1, -1) state.pdims
;;

let getpdimno pageno =
  let rec f p l =
    let np = succ p in
    match l with
    | (n, _, _, _) :: rest ->
        if n >= pageno
        then (if n = pageno then np else p)
        else f np rest

    | [] -> p
  in
  f ~-1 state.pdims
;;

let getpagey pageno = fst (getpageyh pageno);;

let getanchor1 l =
  let top =
    let coloff = l.pagecol * l.pageh in
    float (l.pagey + coloff) /. float l.pageh
  in
  let dtop =
    if l.pagedispy = 0
    then
      0.0
    else (
      if conf.presentation
      then float l.pagedispy /. float (calcips l.pageh)
      else float l.pagedispy /. float conf.interpagespace
    )
  in
  (l.pageno, top, dtop)
;;

let getanchor () =
  match state.layout with
  | l :: _ -> getanchor1 l
  | []     ->
      let n = page_of_y state.y in
      if n = -1
      then state.anchor
      else
        let y, h = getpageyh n in
        let dy = y -. state.y in
        let dtop =
          if conf.presentation
          then
            let ips = calcips h in
            (dy +. float ips) /. float ips
          else
            dy /. float conf.interpagespace
        in
        (n, 0.0, dtop)
;;

let fontpath = ref E.s;;

type historder = [ `lastvisit | `title | `path | `file ];;

module KeyMap =
  Map.Make (struct type t = (int * int) let compare = compare end);;

let unentS s =
  let l = String.length s in
  let b = Buffer.create l in
  Parser.unent b s 0 l;
  Buffer.contents b;
;;

let home =
  try Sys.getenv "HOME"
  with exn ->
    dolog "cannot determine home directory location: %s" @@ exntos exn;
    E.s
;;

let modifier_of_string = function
  | "alt" -> Wsi.altmask
  | "shift" -> Wsi.shiftmask
  | "ctrl" | "control" -> Wsi.ctrlmask
  | "meta" -> Wsi.metamask
  | _ -> 0
;;

let keys_of_string s =
  let key_of_string r s =
    let elems = Str.full_split r s in
    let f n k m =
      let g s =
        let m1 = modifier_of_string s in
        if m1 = 0
        then (Wsi.namekey s, m)
        else (k, m lor m1)
      in function
        | Str.Delim s when n land 1 = 0 -> g s
        | Str.Text s -> g s
        | Str.Delim _ -> (k, m)
    in
    let rec loop n k m = function
      | [] -> (k, m)
      | x :: xs ->
          let k, m = f n k m x in
          loop (n+1) k m xs
    in
    loop 0 0 0 elems
  in
  let elems = Str.split whitere s in
  List.map (key_of_string (Str.regexp "-")) elems
;;

let config_of c attrs =
  let apply c k v =
    try
      match k with
      | "scroll-bar-width" -> { c with scrollbw = max 0 (int_of_string v) }
      | "scroll-handle-height" -> { c with scrollh = max 0 (int_of_string v) }
      | "case-insensitive-search" -> { c with icase = bool_of_string v }
      | "preload" -> { c with preload = bool_of_string v }
      | "page-bias" -> { c with pagebias = int_of_string v }
      | "scroll-step" -> { c with scrollstep = max 1 (int_of_string v) }
      | "horizontal-scroll-step" ->
          { c with hscrollstep = max (int_of_string v) 1 }
      | "auto-scroll-step" ->
          { c with autoscrollstep = max 0 (int_of_string v) }
      | "max-height-fit" -> { c with maxhfit = bool_of_string v }
      | "crop-hack" -> { c with crophack = bool_of_string v }
      | "throttle" ->
          let mw =
            match String.map asciilower v with
            | "true" -> Some infinity
            | "false" -> None
            | f -> Some (float_of_string f)
          in
          { c with maxwait = mw }
      | "highlight-links" -> { c with hlinks = bool_of_string v }
      | "under-cursor-info" -> { c with underinfo = bool_of_string v }
      | "vertical-margin" ->
          { c with interpagespace = max 0 (int_of_string v) }
      | "zoom" ->
          let zoom = float_of_string v /. 100. in
          let zoom = max zoom 0.0 in
          { c with zoom = zoom }
      | "presentation" -> { c with presentation = bool_of_string v }
      | "rotation-angle" -> { c with angle = int_of_string v }
      | "width" -> { c with cwinw = max 20 (int_of_string v) }
      | "height" -> { c with cwinh = max 20 (int_of_string v) }
      | "persistent-bookmarks" -> { c with savebmarks = bool_of_string v }
      | "proportional-display" ->
          let fm =
            if bool_of_string v
            then FitProportional
            else FitWidth
          in
          { c with fitmodel = fm }
      | "fit-model" -> { c with fitmodel = FMTE.of_string v }
      | "pixmap-cache-size" ->
          { c with memlimit = max 2 (int_of_string_with_suffix v) }
      | "tex-count" -> { c with texcount = max 1 (int_of_string v) }
      | "slice-height" -> { c with sliceheight = max 2 (int_of_string v) }
      | "thumbnail-width" -> { c with thumbw = max 2 (int_of_string v) }
      | "persistent-location" -> { c with jumpback = bool_of_string v }
      | "background-color" -> { c with bgcolor = color_of_string v }
      | "tile-width" -> { c with tilew = max 2 (int_of_string v) }
      | "tile-height" -> { c with tileh = max 2 (int_of_string v) }
      | "mupdf-store-size" ->
          { c with mustoresize = max 1024 (int_of_string_with_suffix v) }
      | "checkers" -> { c with checkers = bool_of_string v }
      | "aalevel" -> { c with aalevel = max 0 (int_of_string v) }
      | "trim-margins" -> { c with trimmargins = bool_of_string v }
      | "trim-fuzz" -> { c with trimfuzz = irect_of_string v }
      | "uri-launcher" -> { c with urilauncher = unentS v }
      | "path-launcher" -> { c with pathlauncher = unentS v }
      | "color-space" -> { c with colorspace = CSTE.of_string v }
      | "invert-colors" -> { c with invert = bool_of_string v }
      | "brightness" -> { c with colorscale = float_of_string v }
      | "ghyllscroll" -> { c with ghyllscroll = ghyllscroll_of_string v }
      | "columns" ->
          let (n, _, _) as nab = multicolumns_of_string v in
          if n < 0
          then { c with columns = Csplit (-n, E.a) }
          else { c with columns = Cmulti (nab, E.a) }
      | "birds-eye-columns" ->
          { c with beyecolumns = Some (max (int_of_string v) 2) }
      | "selection-command" -> { c with selcmd = unentS v }
      | "synctex-command" -> { c with stcmd = unentS v }
      | "pax-command" -> { c with paxcmd = unentS v }
      | "askpass-command" -> { c with passcmd = unentS v }
      | "savepath-command" -> { c with savecmd = unentS v }
      | "update-cursor" -> { c with updatecurs = bool_of_string v }
      | "hint-font-size" -> { c with hfsize = bound (int_of_string v) 5 100 }
      | "page-scroll-scale" -> { c with pgscale = float_of_string v }
      | "use-pbo" -> { c with usepbo = bool_of_string v }
      | "wheel-scrolls-pages" -> { c with wheelbypage = bool_of_string v }
      | "horizontal-scrollbar-visible" ->
          let b =
            if bool_of_string v
            then c.scrollb lor scrollbhv
            else c.scrollb land (lnot scrollbhv)
          in
          { c with scrollb = b }
      | "vertical-scrollbar-visible" ->
          let b =
            if bool_of_string v
            then c.scrollb lor scrollbvv
            else c.scrollb land (lnot scrollbvv)
          in
          { c with scrollb = b }
      | "remote-in-a-new-instance" -> { c with riani = bool_of_string v }
      | "point-and-x" ->
          { c with pax =
              if bool_of_string v
              then Some (ref (0.0, 0, 0))
              else None }
      | "point-and-x-mark" -> { c with paxmark = MTE.of_string v }
      | "scroll-bar-on-the-left" -> { c with leftscroll = bool_of_string v }
      | "title" -> { c with title = unentS v }
      | "last-visit" -> { c with lastvisit = float_of_string v }
      | "edit-annotations-inline" -> { c with annotinline = bool_of_string v }
      | "coarse-presentation-positioning" ->
         { c with coarseprespos = bool_of_string v }
      | _ -> c
    with exn ->
      dolog "error processing attribute (`%S' = `%S'): %s" k v @@ exntos exn;
      c
  in
  let rec fold c = function
    | [] -> c
    | (k, v) :: rest ->
        let c = apply c k v in
        fold c rest
  in
  fold { c with keyhashes = copykeyhashes c } attrs;
;;

let fromstring f pos n v d =
  try f v
  with exn ->
    dolog "error processing attribute (%S=%S) at %d\n%s" n v pos @@ exntos exn;
    d
;;

let bookmark_of attrs =
  let rec fold title page rely visy = function
    | ("title", v) :: rest -> fold v page rely visy rest
    | ("page", v) :: rest -> fold title v rely visy rest
    | ("rely", v) :: rest -> fold title page v visy rest
    | ("visy", v) :: rest -> fold title page rely v rest
    | _ :: rest -> fold title page rely visy rest
    | [] -> title, page, rely, visy
  in
  fold "invalid" "0" "0" "0" attrs
;;

let doc_of attrs =
  let rec fold path page rely pan visy origin = function
    | ("path", v) :: rest -> fold v page rely pan visy origin rest
    | ("page", v) :: rest -> fold path v rely pan visy origin rest
    | ("rely", v) :: rest -> fold path page v pan visy origin rest
    | ("pan", v) :: rest -> fold path page rely v visy origin rest
    | ("visy", v) :: rest -> fold path page rely pan v origin rest
    | ("origin", v) :: rest -> fold path page rely pan visy v rest
    | _ :: rest -> fold path page rely pan visy origin rest
    | [] -> path, page, rely, pan, visy, origin
  in
  fold E.s "0" "0" "0" "0" E.s attrs
;;

let map_of attrs =
  let rec fold rs ls = function
    | ("out", v) :: rest -> fold v ls rest
    | ("in", v) :: rest -> fold rs v rest
    | _ :: rest -> fold ls rs rest
    | [] -> ls, rs
  in
  fold E.s E.s attrs
;;

let setconf dst src =
  dst.scrollbw       <- src.scrollbw;
  dst.scrollh        <- src.scrollh;
  dst.icase          <- src.icase;
  dst.preload        <- src.preload;
  dst.pagebias       <- src.pagebias;
  dst.verbose        <- src.verbose;
  dst.scrollstep     <- src.scrollstep;
  dst.maxhfit        <- src.maxhfit;
  dst.crophack       <- src.crophack;
  dst.autoscrollstep <- src.autoscrollstep;
  dst.maxwait        <- src.maxwait;
  dst.hlinks         <- src.hlinks;
  dst.underinfo      <- src.underinfo;
  dst.interpagespace <- src.interpagespace;
  dst.zoom           <- src.zoom;
  dst.presentation   <- src.presentation;
  dst.angle          <- src.angle;
  dst.cwinw          <- src.cwinw;
  dst.cwinh          <- src.cwinh;
  dst.savebmarks     <- src.savebmarks;
  dst.memlimit       <- src.memlimit;
  dst.fitmodel       <- src.fitmodel;
  dst.texcount       <- src.texcount;
  dst.sliceheight    <- src.sliceheight;
  dst.thumbw         <- src.thumbw;
  dst.jumpback       <- src.jumpback;
  dst.bgcolor        <- src.bgcolor;
  dst.tilew          <- src.tilew;
  dst.tileh          <- src.tileh;
  dst.mustoresize    <- src.mustoresize;
  dst.checkers       <- src.checkers;
  dst.aalevel        <- src.aalevel;
  dst.trimmargins    <- src.trimmargins;
  dst.trimfuzz       <- src.trimfuzz;
  dst.urilauncher    <- src.urilauncher;
  dst.colorspace     <- src.colorspace;
  dst.invert         <- src.invert;
  dst.colorscale     <- src.colorscale;
  dst.ghyllscroll    <- src.ghyllscroll;
  dst.columns        <- src.columns;
  dst.beyecolumns    <- src.beyecolumns;
  dst.selcmd         <- src.selcmd;
  dst.updatecurs     <- src.updatecurs;
  dst.pathlauncher   <- src.pathlauncher;
  dst.keyhashes      <- copykeyhashes src;
  dst.hfsize         <- src.hfsize;
  dst.hscrollstep    <- src.hscrollstep;
  dst.pgscale        <- src.pgscale;
  dst.usepbo         <- src.usepbo;
  dst.wheelbypage    <- src.wheelbypage;
  dst.stcmd          <- src.stcmd;
  dst.paxcmd         <- src.paxcmd;
  dst.passcmd        <- src.passcmd;
  dst.savecmd        <- src.savecmd;
  dst.scrollb        <- src.scrollb;
  dst.riani          <- src.riani;
  dst.paxmark        <- src.paxmark;
  dst.leftscroll     <- src.leftscroll;
  dst.title          <- src.title;
  dst.annotinline    <- src.annotinline;
  dst.coarseprespos  <- src.coarseprespos;
  dst.pax            <-
    if src.pax = None
    then None
    else Some ((ref (0.0, 0, 0)));
;;

let findkeyhash c name =
  try List.assoc name c.keyhashes
  with Not_found -> failwith ("invalid mode name `" ^ name ^ "'")
;;

let get s =
  let open Parser in
  let h = Hashtbl.create 10 in
  let dc = { defconf with angle = defconf.angle } in
  let rec toplevel v t spos _ =
    match t with
    | Vdata | Vcdata | Vend -> v
    | Vopen ("llppconfig", _, closed) ->
        if closed
        then v
        else { v with f = llppconfig }
    | Vopen _ ->
        error "unexpected subelement at top level" s spos
    | Vclose _ -> error "unexpected close at top level" s spos

  and llppconfig v t spos _ =
    match t with
    | Vdata | Vcdata -> v
    | Vend -> error "unexpected end of input in llppconfig" s spos
    | Vopen ("defaults", attrs, closed) ->
        let c = config_of dc attrs in
        setconf dc c;
        if closed
        then v
        else { v with f = defaults }

    | Vopen ("ui-font", attrs, closed) ->
        let rec getsize size = function
          | [] -> size
          | ("size", v) :: rest ->
              let size =
                fromstring int_of_string spos "size" v fstate.fontsize in
              getsize size rest
          | l -> getsize size l
        in
        fstate.fontsize <- getsize fstate.fontsize attrs;
        if closed
        then v
        else { v with f = uifont (Buffer.create 10) }

    | Vopen ("doc", attrs, closed) ->
        let pathent, spage, srely, span, svisy, origin = doc_of attrs in
        let path = unentS pathent
        and origin = unentS origin
        and pageno = fromstring int_of_string spos "page" spage 0
        and rely = fromstring float_of_string spos "rely" srely 0.0
        and pan = fromstring int_of_string spos "pan" span 0
        and visy = fromstring float_of_string spos "visy" svisy 0.0 in
        let c = config_of dc attrs in
        let anchor = (pageno, rely, visy) in
        if closed
        then (Hashtbl.add h path (c, [], pan, anchor, origin); v)
        else { v with f = doc path origin pan anchor c [] }

    | Vopen _ ->
        error "unexpected subelement in llppconfig" s spos

    | Vclose "llppconfig" ->  { v with f = toplevel }
    | Vclose _ -> error "unexpected close in llppconfig" s spos

  and defaults v t spos _ =
    match t with
    | Vdata | Vcdata -> v
    | Vend -> error "unexpected end of input in defaults" s spos
    | Vopen ("keymap", attrs, closed) ->
        let modename =
          try List.assoc "mode" attrs
          with Not_found -> "global" in
        if closed
        then v
        else
          let ret keymap =
            let h = findkeyhash dc modename in
            KeyMap.iter (Hashtbl.replace h) keymap;
            defaults
          in
          { v with f = pkeymap ret KeyMap.empty }

    | Vopen (_, _, _) ->
        error "unexpected subelement in defaults" s spos

    | Vclose "defaults" ->
        { v with f = llppconfig }

    | Vclose _ -> error "unexpected close in defaults" s spos

  and uifont b v t spos epos =
    match t with
    | Vdata | Vcdata ->
        Buffer.add_substring b s spos (epos - spos);
        v
    | Vopen (_, _, _) ->
        error "unexpected subelement in ui-font" s spos
    | Vclose "ui-font" ->
        if emptystr !fontpath
        then fontpath := Buffer.contents b;
        { v with f = llppconfig }
    | Vclose _ -> error "unexpected close in ui-font" s spos
    | Vend -> error "unexpected end of input in ui-font" s spos

  and doc path origin pan anchor c bookmarks v t spos _ =
    match t with
    | Vdata | Vcdata -> v
    | Vend -> error "unexpected end of input in doc" s spos
    | Vopen ("bookmarks", _, closed) ->
        if closed
        then v
        else { v with f = pbookmarks path origin pan anchor c bookmarks }

    | Vopen ("keymap", attrs, closed) ->
        let modename =
          try List.assoc "mode" attrs
          with Not_found -> "global"
        in
        if closed
        then v
        else
          let ret keymap =
            let h = findkeyhash c modename in
            KeyMap.iter (Hashtbl.replace h) keymap;
            doc path origin pan anchor c bookmarks
          in
          { v with f = pkeymap ret KeyMap.empty }

    | Vopen (_, _, _) ->
        error "unexpected subelement in doc" s spos

    | Vclose "doc" ->
        Hashtbl.add h path (c, List.rev bookmarks, pan, anchor, origin);
        { v with f = llppconfig }

    | Vclose _ -> error "unexpected close in doc" s spos

  and pkeymap ret keymap v t spos _ =
    match t with
    | Vdata | Vcdata -> v
    | Vend -> error "unexpected end of input in keymap" s spos
    | Vopen ("map", attrs, closed) ->
        let r, l = map_of attrs in
        let kss = fromstring keys_of_string spos "in" r [] in
        let lss = fromstring keys_of_string spos "out" l [] in
        let keymap =
          match kss with
          | [] -> keymap
          | ks :: [] -> KeyMap.add ks (KMinsrl lss) keymap
          | ks :: rest -> KeyMap.add ks (KMmulti (rest, lss)) keymap
        in
        if closed
        then { v with f = pkeymap ret keymap }
        else
          let f () = v in
          { v with f = skip "map" f }

    | Vopen _ ->
        error "unexpected subelement in keymap" s spos

    | Vclose "keymap" ->
        { v with f = ret keymap }

    | Vclose _ -> error "unexpected close in keymap" s spos

  and pbookmarks path origin pan anchor c bookmarks v t spos _ =
    match t with
    | Vdata | Vcdata -> v
    | Vend -> error "unexpected end of input in bookmarks" s spos
    | Vopen ("item", attrs, closed) ->
        let titleent, spage, srely, svisy = bookmark_of attrs in
        let page = fromstring int_of_string spos "page" spage 0
        and rely = fromstring float_of_string spos "rely" srely 0.0
        and visy = fromstring float_of_string spos "visy" svisy 0.0 in
        let bookmarks =
          (unentS titleent, 0, Oanchor (page, rely, visy)) :: bookmarks
        in
        if closed
        then { v with f = pbookmarks path origin pan anchor c bookmarks }
        else
          let f () = v in
          { v with f = skip "item" f }

    | Vopen _ ->
        error "unexpected subelement in bookmarks" s spos

    | Vclose "bookmarks" ->
        { v with f = doc path origin pan anchor c bookmarks }

    | Vclose _ -> Parser.parse_error "unexpected close in bookmarks" s spos

  and skip tag f v t spos _ =
    match t with
    | Vdata | Vcdata -> v
    | Vend ->
        Parser.parse_error ("unexpected end of input in skipped " ^ tag) s spos
    | Vopen (tag', _, closed) ->
        if closed
        then v
        else
          let f' () = { v with f = skip tag f } in
          { v with f = skip tag' f' }
    | Vclose ctag ->
        if tag = ctag
        then f ()
        else Parser.parse_error ("unexpected close in skipped " ^ tag) s spos
  in

  parse { f = toplevel; accu = () } s;
  h, dc;
;;

let do_load f contents =
  try f contents
  with
  | Parser.Parse_error (msg, s, pos) ->
      let subs = Parser.subs s pos in
      Utils.error "parse error: %s: at %d [..%S..]" msg pos subs

  | exn -> Utils.error "parse error: %s" @@ exntos exn
;;

let defconfpath =
  let dir =
    let xdgconfdir = Utils.getenvwithdef "XDG_CONFIG_HOME" E.s in
    if emptystr xdgconfdir
    then
      try
        let dir = Filename.concat home ".config" in
        if Sys.is_directory dir then dir else home
      with _ -> home
    else xdgconfdir
  in
  Filename.concat dir "llpp.conf"
;;

let confpath = ref defconfpath;;

let load2 f default =
  match filecontents !confpath with
  | contents -> f @@ do_load get contents
  | exception Unix.Unix_error (Unix.ENOENT, "open", _) ->
      f (Hashtbl.create 0, defconf)
  | exception exn ->
      dolog "error loading configuration from `%S': %s" !confpath @@ exntos exn;
      default
;;

let load1 f = load2 f false;;

let load openlast =
  let f (h, dc) =
    if openlast
    then (
      let path, _ =
        Hashtbl.fold
          (fun path (conf, _, _, _, _) ((_, besttime) as best) ->
            if conf.lastvisit > besttime
            then (path, conf.lastvisit)
            else best)
          h
          (state.path, -.infinity)
      in
      state.path <- path;
     );
    let pc, pb, px, pa, po =
      try
        let absname = abspath state.path in
        Hashtbl.find h absname
      with Not_found -> dc, [], 0, emptyanchor, state.origin
    in
    setconf defconf dc;
    setconf conf pc;
    state.bookmarks <- pb;
    state.x <- float px;
    state.origin <- po;
    if conf.jumpback
    then state.anchor <- pa;
    cbput state.hists.nav pa;
    true
  in
  load1 f
;;

let gethist () =
  let f (h, _) =
    Hashtbl.fold (fun path (pc, pb, px, pa, po) accu ->
      (path, pc, pb, float px, pa, po) :: accu)
      h [];
  in
  load2 f []
;;

let add_attrs bb always dc c time =
  let o' fmt s =
    Buffer.add_string bb "\n    ";
    Printf.bprintf bb fmt s
  in
  let o c fmt s = if c then o' fmt s else ignore in
  let ob s a b = o (always || a != b) "%s='%b'" s a
  and op s a b = o (always || a <> b) "%s='%b'" s (a != None)
  and oi s a b = o (always || a != b) "%s='%d'" s a
  and oI s a b = o (always || a != b) "%s='%s'" s (string_with_suffix_of_int a)
  and oz s a b = o (always || a <> b) "%s='%g'" s (a*.100.)
  and oF s a b = o (always || a <> b) "%s='%f'" s a
  and oL s a b = o (always || a <> b) "%s='%Ld'" s a
  and oc s a b = o (always || a <> b) "%s='%s'" s (color_to_string a)
  and oC s a b = o (always || a <> b) "%s='%s'" s (CSTE.to_string a)
  and oR s a b = o (always || a <> b) "%s='%s'" s (irect_to_string a)
  and oFm s a b = o (always || a <> b) "%s='%s'" s (FMTE.to_string a)
  and oSv s a b m = o (always || a <> b) "%s='%b'" s (a land m != 0)
  and oPm s a b = o (always || a <> b) "%s='%s'" s (MTE.to_string a)
  and os s a b =
    o (always || a <> b) "%s='%s'" s @@ Parser.enent a 0 (String.length a)
  and og s a b =
    if always || a <> b
    then
      match a with
      | Some (_N, _A, _B) -> o' "%s='%u,%u,%u'" s _N _A _B
      | None ->
          match b with
          | None -> ()
          | _ -> o' "%s='none'" s
  and oW s a b =
    if always || a <> b
    then
      let v =
        match a with
        | None -> "false"
        | Some f ->
            if f = infinity
            then "true"
            else string_of_float f
      in
      o' "%s='%s'" s v
  and oco s a b =
    if always || a <> b
    then
      match a with
      | Cmulti ((n, a, b), _) when n > 1 -> o' "%s='%d,%d,%d'" s n a b
      | Csplit (n, _) when n > 1 -> o' "%s='%d'" s ~-n
      | Cmulti _ | Csplit _ | Csingle _ -> ()
  and obeco s a b =
    if always || a <> b
    then
      match a with
      | Some c when c > 1 -> o' "%s='%d'" s c
      | _ -> ()
  in
  oi "width" c.cwinw dc.cwinw;
  oi "height" c.cwinh dc.cwinh;
  oi "scroll-bar-width" c.scrollbw dc.scrollbw;
  oi "scroll-handle-height" c.scrollh dc.scrollh;
  oSv "horizontal-scrollbar-visible" c.scrollb dc.scrollb scrollbhv;
  oSv "vertical-scrollbar-visible" c.scrollb dc.scrollb scrollbvv;
  ob "case-insensitive-search" c.icase dc.icase;
  ob "preload" c.preload dc.preload;
  oi "page-bias" c.pagebias dc.pagebias;
  oi "scroll-step" c.scrollstep dc.scrollstep;
  oi "auto-scroll-step" c.autoscrollstep dc.autoscrollstep;
  ob "max-height-fit" c.maxhfit dc.maxhfit;
  ob "crop-hack" c.crophack dc.crophack;
  oW "throttle" c.maxwait dc.maxwait;
  ob "highlight-links" c.hlinks dc.hlinks;
  ob "under-cursor-info" c.underinfo dc.underinfo;
  oi "vertical-margin" c.interpagespace dc.interpagespace;
  oz "zoom" c.zoom dc.zoom;
  ob "presentation" c.presentation dc.presentation;
  oi "rotation-angle" c.angle dc.angle;
  ob "persistent-bookmarks" c.savebmarks dc.savebmarks;
  oFm "fit-model" c.fitmodel dc.fitmodel;
  oI "pixmap-cache-size" c.memlimit dc.memlimit;
  oi "tex-count" c.texcount dc.texcount;
  oi "slice-height" c.sliceheight dc.sliceheight;
  oi "thumbnail-width" c.thumbw dc.thumbw;
  ob "persistent-location" c.jumpback dc.jumpback;
  oc "background-color" c.bgcolor dc.bgcolor;
  oi "tile-width" c.tilew dc.tilew;
  oi "tile-height" c.tileh dc.tileh;
  oI "mupdf-store-size" c.mustoresize dc.mustoresize;
  ob "checkers" c.checkers dc.checkers;
  oi "aalevel" c.aalevel dc.aalevel;
  ob "trim-margins" c.trimmargins dc.trimmargins;
  oR "trim-fuzz" c.trimfuzz dc.trimfuzz;
  os "uri-launcher" c.urilauncher dc.urilauncher;
  os "path-launcher" c.pathlauncher dc.pathlauncher;
  oC "color-space" c.colorspace dc.colorspace;
  ob "invert-colors" c.invert dc.invert;
  oF "brightness" c.colorscale dc.colorscale;
  og "ghyllscroll" c.ghyllscroll dc.ghyllscroll;
  oco "columns" c.columns dc.columns;
  obeco "birds-eye-columns" c.beyecolumns dc.beyecolumns;
  os "selection-command" c.selcmd dc.selcmd;
  os "synctex-command" c.stcmd dc.stcmd;
  os "pax-command" c.paxcmd dc.paxcmd;
  os "askpass-command" c.passcmd dc.passcmd;
  os "savepath-command" c.savecmd dc.savecmd;
  ob "update-cursor" c.updatecurs dc.updatecurs;
  oi "hint-font-size" c.hfsize dc.hfsize;
  oi "horizontal-scroll-step" c.hscrollstep dc.hscrollstep;
  oF "page-scroll-scale" c.pgscale dc.pgscale;
  ob "use-pbo" c.usepbo dc.usepbo;
  ob "wheel-scrolls-pages" c.wheelbypage dc.wheelbypage;
  ob "remote-in-a-new-instance" c.riani dc.riani;
  op "point-and-x" c.pax dc.pax;
  oPm "point-and-x-mark" c.paxmark dc.paxmark;
  ob "scroll-bar-on-the-left" c.leftscroll dc.leftscroll;
  if not always
  then os "title" c.title dc.title;
  oL "last-visit" (Int64.of_float time) 0L;
  ob "edit-annotations-inline" c.annotinline dc.annotinline;
  ob "coarse-presentation-positioning" c.coarseprespos dc.coarseprespos;
;;

let keymapsbuf always dc c =
  let bb = Buffer.create 16 in
  let rec loop = function
    | [] -> ()
    | (modename, h) :: rest ->
        let dh = findkeyhash dc modename in
        if always || h <> dh
        then (
          if Hashtbl.length h > 0
          then (
            if Buffer.length bb > 0
            then Buffer.add_char bb '\n';
            Printf.bprintf bb "<keymap mode='%s'>\n" modename;
            Hashtbl.iter (fun i o ->
              let isdifferent = always ||
                try
                  let dO = Hashtbl.find dh i in
                  dO <> o
                with Not_found -> true
              in
              if isdifferent
              then
                let addkm (k, m) =
                  if Wsi.withctrl m  then Buffer.add_string bb "ctrl-";
                  if Wsi.withalt m   then Buffer.add_string bb "alt-";
                  if Wsi.withshift m then Buffer.add_string bb "shift-";
                  if Wsi.withmeta m  then Buffer.add_string bb "meta-";
                  Buffer.add_string bb (Wsi.keyname k);
                in
                let addkms l =
                  let rec loop = function
                    | [] -> ()
                    | km :: [] -> addkm km
                    | km :: rest -> addkm km; Buffer.add_char bb ' '; loop rest
                  in
                  loop l
                in
                Buffer.add_string bb "<map in='";
                addkm i;
                match o with
                | KMinsrt km ->
                    Buffer.add_string bb "' out='";
                    addkm km;
                    Buffer.add_string bb "'/>\n"

                | KMinsrl kms ->
                    Buffer.add_string bb "' out='";
                    addkms kms;
                    Buffer.add_string bb "'/>\n"

                | KMmulti (ins, kms) ->
                    Buffer.add_char bb ' ';
                    addkms ins;
                    Buffer.add_string bb "' out='";
                    addkms kms;
                    Buffer.add_string bb "'/>\n"
            ) h;
            Buffer.add_string bb "</keymap>";
          );
        );
        loop rest
  in
  loop c.keyhashes;
  bb;
;;

let save1 bb leavebirdseye x h dc =
  let uifontsize = fstate.fontsize in
  let dc = if conf.bedefault then conf else dc in
  Buffer.add_string bb "<llppconfig>\n";

  if nonemptystr !fontpath
  then
    Printf.bprintf bb "<ui-font size='%d'><![CDATA[%s]]></ui-font>\n"
      uifontsize
      !fontpath
  else (
    if uifontsize <> 14
    then
      Printf.bprintf bb "<ui-font size='%d'/>\n" uifontsize
   );

  Buffer.add_string bb "<defaults";
  add_attrs bb true dc dc nan;
  let kb = keymapsbuf true dc dc in
  if Buffer.length kb > 0
  then (
    Buffer.add_string bb ">\n";
    Buffer.add_buffer bb kb;
    Buffer.add_string bb "\n</defaults>\n";
   )
  else Buffer.add_string bb "/>\n";

  let adddoc path pan anchor c bookmarks time origin =
    if bookmarks == [] && c = dc && anchor = emptyanchor
    then ()
    else (
      Printf.bprintf bb "<doc path='%s'"
        (Parser.enent path 0 (String.length path));

      if nonemptystr origin
      then
        Printf.bprintf bb "\n    origin='%s'"
          (Parser.enent origin 0 (String.length origin));

      if anchor <> emptyanchor
      then (
        let n, rely, visy = anchor in
        Printf.bprintf bb "\n    page='%d'" n;
        if rely > 1e-6
        then
          Printf.bprintf bb " rely='%f'" rely
            ;
        if abs_float visy > 1e-6
        then
          Printf.bprintf bb " visy='%f'" visy
            ;
       );

      if pan != 0
      then Printf.bprintf bb " pan='%d'" pan;

      add_attrs bb false dc c time;
      let kb = keymapsbuf false dc c in

      begin match bookmarks with
      | [] ->
          if Buffer.length kb > 0
          then (
            Buffer.add_string bb ">\n";
            Buffer.add_buffer bb kb;
            Buffer.add_string bb "\n</doc>\n";
           )
          else Buffer.add_string bb "/>\n"
      | _ ->
          Buffer.add_string bb ">\n<bookmarks>\n";
          List.iter (fun (title, _, kind) ->
            begin match kind with
            | Oanchor (page, rely, visy) ->
                Printf.bprintf bb
                  "<item title='%s' page='%d'"
                  (Parser.enent title 0 (String.length title))
                  page
                  ;
                if rely > 1e-6
                then
                  Printf.bprintf bb " rely='%f'" rely
                    ;
                if abs_float visy > 1e-6
                then
                  Printf.bprintf bb " visy='%f'" visy
                    ;
            | Ohistory _ | Onone | Ouri _ | Oremote _
            | Oremotedest _ | Olaunch _ ->
                failwith "unexpected link in bookmarks"
            end;
            Buffer.add_string bb "/>\n";
                    ) bookmarks;
          Buffer.add_string bb "</bookmarks>";
          if Buffer.length kb > 0
          then (
            Buffer.add_string bb "\n";
            Buffer.add_buffer bb kb;
           );
          Buffer.add_string bb "\n</doc>\n";
      end;
     )
  in

  let pan, conf =
    match state.mode with
    | Birdseye (c, pan, _, _, _) ->
        let beyecolumns =
          match conf.columns with
          | Cmulti ((c, _, _), _) -> Some c
          | Csingle _ -> None
          | Csplit _ -> None
        and columns =
          match c.columns with
          | Cmulti (c, _) -> Cmulti (c, E.a)
          | Csingle _ -> Csingle E.a
          | Csplit _ -> failwith "quit from bird's eye while split"
        in
        pan, { c with beyecolumns = beyecolumns; columns = columns }
    | Textentry _
    | View
    | LinkNav _ -> x, conf
  in
  let docpath = if nonemptystr state.path then abspath state.path else E.s in
  if nonemptystr docpath
  then (
    adddoc docpath pan (getanchor ())
      (
       let autoscrollstep =
         match state.autoscroll with
         | Some step -> step
         | None -> conf.autoscrollstep
       in
       begin match state.mode with
       | Birdseye beye -> leavebirdseye beye true
       | Textentry _
       | View
       | LinkNav _ -> ()
       end;
       { conf with autoscrollstep = autoscrollstep }
      )
      (if conf.savebmarks then state.bookmarks else [])
      (now ())
      state.origin
  );
  Hashtbl.iter (fun path (c, bookmarks, x, anchor, origin) ->
    if docpath <> abspath path
    then adddoc path x anchor c bookmarks c.lastvisit origin
               ) h;
  Buffer.add_string bb "</llppconfig>\n";
  true;
;;

let save leavebirdseye =
  let relx = state.x /. float state.winw in
  let w, h, x =
    let cx w = relx *. float w in
    List.fold_left
      (fun (w, h, x) ws ->
        match ws with
        | Wsi.Fullscreen -> (conf.cwinw, conf.cwinh, cx conf.cwinw)
        | Wsi.MaxVert -> (w, conf.cwinh, x)
        | Wsi.MaxHorz -> (conf.cwinw, h, cx conf.cwinw)
      )
      (state.winw, state.winh, state.x) state.winstate
  in
  conf.cwinw <- w;
  conf.cwinh <- h;
  let bb = Buffer.create 32768 in
  let save2 (h, dc) =
    save1 bb leavebirdseye (truncate x) h dc
  in
  if load1 save2 && Buffer.length bb > 0
  then
    try
      let tmp = !confpath ^ ".tmp" in
      let oc = open_out_bin tmp in
      Buffer.output_buffer oc bb;
      close_out oc;
      Unix.rename tmp !confpath;
    with exn ->
      dolog "error saving configuration: %s" @@ exntos exn
;;

let gc fd =
  let wr s n =
    (* This here has a potential of rising SIGPIPE, silently and
       seemingly harmlessly (when -gc was supplied an invalid
       executable for instance) probably needs revisiting *)
    let n' = Unix.write fd (Bytes.of_string s) 0 n in
    if n != n'
    then Utils.error "Unix.write %d = %d" n n'
  in
  let href = ref (Hashtbl.create 0) in
  let cref = ref defconf in
  let push (h, dc) =
    let f path (pc, _pb, _px, _pa, _po) =
      let s =
        Printf.sprintf "%s\000%ld\000" path (Int32.of_float pc.lastvisit)
      in
      wr s (String.length s)
    in
    Hashtbl.iter f h;
    href := h;
    cref := dc;
    true
  in
  ignore (load1 push);
  Unix.shutdown fd Unix.SHUTDOWN_SEND;
  let s = fdcontents fd in
  let rec f ppos =
    match String.index_from s ppos '\000' with
    | exception Not_found -> ()
    | zpos1 ->
        match String.index_from s (zpos1+1) '\000' with
        | exception Not_found -> error "invalid gc input in (%S) at %d" s zpos1
        | zpos2 ->
            let okey = StringLabels.sub s ~pos:ppos ~len:(zpos1-ppos) in
            let nkey = StringLabels.sub s ~pos:(zpos1+1) ~len:(zpos2-zpos1-1) in
            if emptystr nkey
            then (Hashtbl.remove !href okey; f (zpos2+1))
            else
              match Hashtbl.find !href okey with
              | exception Not_found -> Utils.error "gc: cannot find %S" okey
              | v ->
                  Hashtbl.remove !href okey;
                  Hashtbl.replace !href nkey v;
                  f (zpos2+1)
  in
  f 0;
  let bb = Buffer.create 32768 in
  let save2 (_h, dc) = save1 bb (fun _ _ -> ()) 0 !href dc in
  if load1 save2 && Buffer.length bb > 0
  then (
    try
      let tmp = !confpath ^ ".tmp" in
      let oc = open_out_bin tmp in
      Buffer.output_buffer oc bb;
      close_out oc;
      Unix.rename tmp !confpath;
    with exn ->
      dolog "error saving configuration: %s" @@ exntos exn
   );
;;

let logcurrently = function
  | Idle -> dolog "Idle"
  | Loading (l, gen) ->
      dolog "Loading %d gen=%d curgen=%d" l.pageno gen state.gen
  | Tiling (l, pageopaque, colorspace, angle, gen, col, row, tilew, tileh) ->
      dolog
        "Tiling %d[%d,%d] page=%s cs=%s angle=%d"
        l.pageno col row (~> pageopaque)
        (CSTE.to_string colorspace) angle
      ;
      dolog "gen=(%d,%d) (%d,%d) tile=(%d,%d) (%d,%d)"
        angle gen conf.angle state.gen
        tilew tileh
        conf.tilew conf.tileh
      ;
  | Outlining _ ->
      dolog "outlining"
;;
