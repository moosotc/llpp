open Utils;;

let debug = true

type cursor =
  | CURSOR_INHERIT
  | CURSOR_INFO
  | CURSOR_CYCLE
  | CURSOR_FLEUR
  | CURSOR_TEXT

type winstate =
  | MaxVert
  | MaxHorz
  | Fullscreen

type visiblestate =
  | Unobscured
  | PartiallyObscured
  | FullyObscured

let classify_key key =
  let open Keys in
  match key with
  | 0xF704 -> F1
  | 0xF706 -> F3
  | 0xF70C -> F9
  | 62 -> Gt
  | 91 -> Lb
  | 60 -> Lt
  | 93 -> Rb
  | 39 -> Apos
  | 8 -> Backspace
  | 0x7f -> Delete
  | 0xF701 -> Down
  | 13 -> Enter
  | 61 -> Equals
  | 27 -> Escape
  | 0xF729 -> Home
  | 0xF727 -> Insert
  | 0xF72B -> Jend
  | 46 -> KPdelete
  | 0xfff01 -> KPdown
  | 0xfff02 -> KPend
  | 0xfff03 -> KPenter
  | 0xfff04 -> KPhome
  | 0xfff05 -> KPleft
  | 0xfff06 -> KPminus
  | 0xfff07 -> KPnext
  | 0xfff08 -> KPplus
  | 0xfff09 -> KPprior
  | 0xfff0a -> KPright
  | 0xfff0b -> KPup
  | 0xF702 -> KPleft
  | 45 -> Minus
  | 0xF72D -> Next
  | 124 -> Pipe
  | 43 -> Plus
  | 0xF72C -> Prior
  | 63 -> Question
  | 0xF703 -> Right
  | 47 -> Slash
  | 32 -> Space
  | 9 -> Tab
  | 0x7e -> Tilde
  | 0xF700 -> Up
  | 48 -> N0
  | 49 -> N1
  | 50 -> N2
  | 51 -> N3
  | 52 -> N4
  | 57 -> N9
  | 66 -> CB
  | 70 -> CF
  | 71 -> CG
  | 72 -> CH
  | 78 -> CN
  | 80 -> CP
  | 81 -> CQ
  | 87 -> CW
  | 83 -> CS
  | 97 -> Ca
  | 98 -> Cb
  | 99 -> Cc
  | 101 -> Ce
  | 102 -> Cf
  | 103 -> Cg
  | 104 -> Ch
  | 105 -> Ci
  | 106 -> Cj
  | 107 -> Ck
  | 108 -> Cl
  | 109 -> Cm
  | 110 -> Cn
  | 111 -> Co
  | 112 -> Cp
  | 113 -> Cq
  | 114 -> Cr
  | 115 -> Cs
  | 116 -> Ct
  | 117 -> Cu
  | 118 -> Cv
  | 119 -> Cw
  | 120 -> Cx
  | 121 -> Cy
  | 122 -> Cz
  | _ -> Unrecognized
;;

class type t = object
  method map      : bool -> unit
  method expose   : unit
  method visible  : visiblestate -> unit
  method reshape  : int -> int -> unit
  method mouse    : int -> bool -> int -> int -> int -> unit
  method motion   : int -> int -> unit
  method pmotion  : int -> int -> unit
  method key      : int -> int -> unit
  method enter    : int -> int -> unit
  method leave    : unit
  method winstate : winstate list -> unit
  method quit     : 'a. 'a
end

let onot = object
  method map _           = ()
  method expose          = ()
  method visible _       = ()
  method reshape _ _     = ()
  method mouse _ _ _ _ _ = ()
  method motion _ _      = ()
  method pmotion _ _     = ()
  method key _ _         = ()
  method enter _ _       = ()
  method leave           = ()
  method winstate _      = ()
  method quit: 'a. 'a = exit 0
end

type state =
  {
    mutable t: t;
    mutable fd: Unix.file_descr;
  }

let state =
  {
    t = onot;
    fd = Unix.stdin;
  }

let readstr sock n = try readstr sock n with End_of_file -> state.t#quit

external setcursor: cursor -> unit = "ml_setcursor"

external settitle: string -> unit = "ml_settitle"

external swapb: unit -> unit = "ml_swapb"

external reshape: int -> int -> unit = "ml_reshape"

(* 0 -> map
   1 -> expose
   2 -> visible
   3 -> reshape
   4 -> mouse
   5 -> motion
   6 -> pmotion
   7 -> key
   8 -> enter
   9 -> leave
   10 -> winstate
   11 -> quit
   13 -> response *)

let readresp sock =
  let resp = readstr sock 32 in
  let opcode = r8 resp 0 in
  match opcode with
  | 0 ->
    let mapped = r8 resp 16 <> 0 in
    vlog "map %B" mapped;
    state.t#map mapped
  | 1 ->
    vlog "expose";
    state.t#expose
  | 3 ->
    let w = r16 resp 16 in
    let h = r16 resp 18 in
    vlog "reshape width %d height %d" w h;
    state.t#reshape w h
  | 4 ->
    let down = r16 resp 10 <> 0 in
    let b = r32 resp 12 in
    let x = r16s resp 16 in
    let y = r16s resp 20 in
    let m = r32 resp 24 in
    vlog "mouse %s b %d x %d y %d m 0x%x" (if down then "down" else "up") b x y m;
    state.t#mouse b down x y m
  | 5 ->
    let x = r16s resp 16 in
    let y = r16s resp 20 in
    let m = r32 resp 24 in
    vlog "motion x %d y %d m 0x%x" x y m;
    state.t#motion x y
  | 6 ->
    let x = r16s resp 16 in
    let y = r16s resp 20 in
    let m = r32 resp 24 in
    vlog "pmotion x %d y %d m 0x%x" x y m;
    state.t#pmotion x y
  | 7 ->
    let key = r32 resp 16 in
    let mask = r32 resp 20 in
    vlog "keydown key %d mask %d" key mask;
    state.t#key key mask
  | 8 ->
    let x = r16s resp 16 in
    let y = r16s resp 20 in
    vlog "enter x %d y %d" x y;
    state.t#enter x y
  | 9 ->
    vlog "leave";
    state.t#leave
  | 10 ->
    let x = r32 resp 16 <> 0 in
    vlog "winstate %B" x;
    state.t#winstate (if x then [Fullscreen] else []);
  | 11 ->
    vlog "quit";
    state.t#quit
  | _ ->
    vlog "unknown server message %d" opcode

external completeinit: int -> int -> unit = "ml_completeinit"

external file_descr_of_int: int -> Unix.file_descr = "%identity"

external getw: unit -> int = "ml_getw"
external geth: unit -> int = "ml_geth"

let fontsizefactor () =
  int_of_string (Sys.getenv "BACKING_SCALE_FACTOR")

let init t _ w h platform =
  let fd = int_of_string (Sys.getenv "LLPP_DISPLAY") in
  Printf.eprintf "LLPP_DISPLAY=%d\n%!" fd;
  let fd = file_descr_of_int fd in
  state.t <- t;
  state.fd <- fd;
  completeinit w h;
  fd, getw (), geth ()

external fullscreen: unit -> unit = "ml_fullscreen"

let activatewin () = ()

external mapwin: unit -> unit = "ml_mapwin"

let metamask = 1 lsl 19

let altmask = 1 lsl 19

let shiftmask = 1 lsl 17

let ctrlmask = 1 lsl 18

let withalt mask = mask land metamask != 0

let withctrl mask = mask land ctrlmask != 0

let withshift mask = mask land shiftmask != 0

let withmeta mask = mask land metamask != 0

let withnone mask = mask land (altmask + ctrlmask + shiftmask + metamask) = 0

let keyname key = Printf.sprintf "0x%x" key

let namekey s = int_of_string s

external setwinbgcol: int -> unit = "ml_setwinbgcol"

let keypadtodigitkey key = (* FIXME *)
  if key >= 0xffb0 && key <= 0xffb9 (* keypad numbers *)
  then key - 0xffb0 + 48 else key
;;

let isspecialkey key =
  (0x0 <= key && key <= 0x1F) || key = 0x7f || (0x80 <= key && key <= 0x9F)
  || (key land 0xf700 = 0xf700)
;;
