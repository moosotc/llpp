open Utils;;
open Config;;

exception Quit;;

external init : Unix.file_descr -> params -> unit = "ml_init";;
external seltext : opaque -> (int * int * int * int) -> unit = "ml_seltext";;
external hassel : opaque -> bool = "ml_hassel";;
external copysel : Unix.file_descr -> opaque -> unit = "ml_copysel";;
external getpdimrect : int -> float array = "ml_getpdimrect";;
external whatsunder : opaque -> int -> int -> under = "ml_whatsunder";;
external markunder : opaque -> int -> int -> mark -> bool = "ml_markunder";;
external clearmark : opaque -> unit = "ml_clearmark";;
external zoomforh : int -> int -> int -> int -> float = "ml_zoom_for_height";;
external getmaxw : unit -> float = "ml_getmaxw";;
external drawstr : int -> int -> int -> string -> float = "ml_draw_string";;
external measurestr : int -> string -> float = "ml_measure_string";;
external postprocess :
  opaque -> int -> int -> int -> (int * string * int) -> int
  = "ml_postprocess";;
external pagebbox : opaque -> (int * int * int * int) = "ml_getpagebox";;
external setaalevel : int -> unit = "ml_setaalevel";;
external realloctexts : int -> bool = "ml_realloctexts";;
external findlink : opaque -> linkdir -> link = "ml_findlink";;
external getlink : opaque -> int -> under = "ml_getlink";;
external getlinkrect : opaque -> int -> irect = "ml_getlinkrect";;
external getlinkcount : opaque -> int = "ml_getlinkcount";;
external findpwl : int -> int -> pagewithlinks = "ml_find_page_with_links";;
external getpbo : width -> height -> colorspace -> opaque = "ml_getpbo";;
external freepbo : opaque -> unit = "ml_freepbo";;
external unmappbo : opaque -> unit = "ml_unmappbo";;
external bousable : unit -> bool = "ml_bo_usable";;
external unproject : opaque -> int -> int -> (int * int) option
  = "ml_unproject";;
external project : opaque -> int -> int -> float -> float -> (float * float)
  = "ml_project";;
external drawtile : tileparams -> opaque -> unit = "ml_drawtile";;
external rectofblock : opaque -> int -> int -> float array option
  = "ml_rectofblock";;
external begintiles : unit -> unit = "ml_begintiles";;
external endtiles : unit -> unit = "ml_endtiles";;
external addannot : opaque -> int -> int -> string -> unit = "ml_addannot";;
external modannot : opaque -> slinkindex -> string -> unit = "ml_modannot";;
external delannot : opaque -> slinkindex -> unit = "ml_delannot";;
external hasunsavedchanges : unit -> bool = "ml_hasunsavedchanges";;
external savedoc : string -> unit = "ml_savedoc";;
external getannotcontents : opaque -> slinkindex -> string
  = "ml_getannotcontents";;
external drawprect : opaque -> int -> int -> float array -> unit =
  "ml_drawprect";;
external wcmd : Unix.file_descr -> bytes -> int -> unit = "ml_wcmd";;
external rcmd : Unix.file_descr -> string = "ml_rcmd";;

let selfexec = ref E.s;;
let opengl_has_pbo = ref false;;

let drawstring size x y s =
  Gl.enable `blend;
  Gl.enable `texture_2d;
  GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
  ignore (drawstr size x y s);
  Gl.disable `blend;
  Gl.disable `texture_2d;
;;

let drawstring1 size x y s =
  drawstr size x y s;
;;

let drawstring2 size x y fmt =
  Printf.kprintf (drawstring size (x+1) (y+size+1)) fmt
;;

let _debugl l =
  dolog "l %d dim=%d {" l.pageno l.pagedimno;
  dolog "  WxH     %dx%d" l.pagew l.pageh;
  dolog "  vWxH    %dx%d" l.pagevw l.pagevh;
  dolog "  pagex,y %d,%d" l.pagex l.pagey;
  dolog "  dispx,y %d,%d" l.pagedispx l.pagedispy;
  dolog "  column  %d" l.pagecol;
  dolog "}";
;;

let debugrect (x0, y0, x1, y1, x2, y2, x3, y3) =
  dolog "rect {";
  dolog "  x0,y0=(% f, % f)" x0 y0;
  dolog "  x1,y1=(% f, % f)" x1 y1;
  dolog "  x2,y2=(% f, % f)" x2 y2;
  dolog "  x3,y3=(% f, % f)" x3 y3;
  dolog "}";
;;

let isbirdseye = function
  | Birdseye _ -> true
  | Textentry _
  | View
  | LinkNav _ -> false
;;

let istextentry = function
  | Textentry _ -> true
  | Birdseye _
  | View
  | LinkNav _ -> false
;;

let wtmode = ref false;;
let cxack = ref false;;

let pgscale h = truncate (float h *. conf.pgscale);;

let hscrollh () =
  if state.uioh#alwaysscrolly || ((conf.scrollb land scrollbhv != 0)
                                  && (state.w > state.winw))
  then conf.scrollbw
  else 0
;;

let vscrollw () =
  if state.uioh#alwaysscrolly || ((conf.scrollb land scrollbvv != 0)
                                  && (state.maxy > float state.winh))
  then conf.scrollbw
  else 0
;;

let vscrollhit x =
  if conf.leftscroll
  then x < vscrollw ()
  else x > state.winw - vscrollw ()
;;

let setfontsize n =
  fstate.fontsize <- n;
  fstate.wwidth <- measurestr fstate.fontsize "w";
  fstate.maxrows <- (state.winh - fstate.fontsize - 1) / (fstate.fontsize + 1);
;;

let vlog fmt =
  if conf.verbose
  then dolog fmt
  else Printf.kprintf ignore fmt
;;

let launchpath () =
  if emptystr conf.pathlauncher
  then dolog "%s" state.path
  else (
    let command = Str.global_replace percentsre state.path conf.pathlauncher in
    match spawn command [] with
    | _pid -> ()
    | (exception exn) ->
       dolog "failed to execute `%s': %s" command @@ exntos exn
  );
;;

module G =
struct
  let postRedisplay who =
    vlog "redisplay for [%S]" who;
    state.redisplay <- true;
  ;;
end;;

let getopaque pageno =
  try Some (Hashtbl.find state.pagemap (pageno, state.gen))
  with Not_found -> None
;;

let pagetranslatepoint l x y =
  let dy = y - l.pagedispy in
  let y = dy + l.pagey in
  let dx = x - l.pagedispx in
  let x = dx + l.pagex in
  (x, y);
;;

let onppundermouse g x y d =
  let rec f = function
    | l :: rest ->
        begin match getopaque l.pageno with
        | Some opaque ->
            let x0 = l.pagedispx in
            let x1 = x0 + l.pagevw in
            let y0 = l.pagedispy in
            let y1 = y0 + l.pagevh in
            if y >= y0 && y <= y1 && x >= x0 && x <= x1
            then
              let px, py = pagetranslatepoint l x y in
              match g opaque l px py with
              | Some res -> res
              | None -> f rest
            else f rest
        | _ ->
            f rest
        end
    | [] -> d
  in
  f state.layout
;;

let getunder x y =
  let g opaque l px py =
    if state.bzoom
    then (
      match rectofblock opaque px py with
      | Some [|x0;x1;y0;y1|] ->
         let rect = (x0, y0, x1, y0, x1, y1, x0, y1) in
         let color = (0.0, 0.0, 1.0 /. (l.pageno mod 3 |> float), 0.5) in
         state.rects <- [l.pageno, color, rect];
         G.postRedisplay "getunder";
      | _otherwise -> ()
     );
    let under = whatsunder opaque px py in
    if under = Unone then None else Some under
  in
  onppundermouse g x y Unone
;;

let unproject x y =
  let g opaque l x y =
    match unproject opaque x y with
    | Some (x, y) -> Some (Some (opaque, l.pageno, x, y))
    | None -> None
  in
  onppundermouse g x y None;
;;

let showtext c s =
  state.text <- Printf.sprintf "%c%s" c s;
  G.postRedisplay "showtext";
;;

let impmsg fmt =
  Format.ksprintf (fun s -> showtext '!' s) fmt;
;;

let pipesel opaque cmd =
  if hassel opaque
  then
    match Unix.pipe () with
    | (exception exn) -> dolog "pipesel cannot create pipe: %S" @@ exntos exn;
    | (r, w) ->
        let doclose what fd =
          Ne.clo fd (fun msg -> dolog "%s close failed: %s" what msg)
        in
        let pid =
          try spawn cmd [r, 0; w, -1]
          with exn ->
            dolog "cannot execute %S: %s" cmd @@ exntos exn;
            -1
        in
        if pid > 0
        then (
          copysel w opaque;
          G.postRedisplay "pipesel";
        )
        else doclose "pipesel pipe/w" w;
        doclose "pipesel pipe/r" r;
;;

let paxunder x y =
  let g opaque l px py =
    if markunder opaque px py conf.paxmark
    then (
      Some (fun () ->
        match getopaque l.pageno with
        | None -> ()
        | Some opaque -> pipesel opaque conf.paxcmd
      )
    )
    else None
  in
  G.postRedisplay "paxunder";
  if conf.paxmark = Mark_page
  then
    List.iter (fun l ->
      match getopaque l.pageno with
      | None -> ()
      | Some opaque -> clearmark opaque) state.layout;
  state.roam <- onppundermouse g x y (fun () -> impmsg "whoopsie daisy");
;;

let selstring s =
  match Unix.pipe () with
  | (exception exn) -> impmsg "pipe failed: %s" @@ exntos exn
  | (r, w) ->
      let clo cap fd =
        Ne.clo fd (fun msg -> impmsg "failed to close %s: %s" cap msg)
      in
      let pid =
        try spawn conf.selcmd [r, 0; w, -1]
        with exn ->
          impmsg "failed to execute %s: %s" conf.selcmd @@ exntos exn;
          -1
      in
      if pid > 0
      then (
        try
          let l = String.length s in
          let bytes = Bytes.unsafe_of_string s in
          let n = tempfailureretry (Unix.write w bytes 0) l in
          if n != l
          then impmsg "failed to write %d characters to sel pipe, wrote %d"
                      l n
        with exn ->
          impmsg "failed to write to sel pipe: %s" @@ exntos exn
      )
      else dolog "%s" s;
      clo "selstring pipe/r" r;
      clo "selstring pipe/w" w;
;;

let undertext ?(nopath=false) = function
  | Unone -> "none"
  | Ulinkuri s -> s
  | Ulinkgoto (pageno, _) ->
      if nopath
      then "page " ^ string_of_int (pageno+1)
      else Printf.sprintf "%s: page %d" state.path (pageno+1)
  | Utext s -> "font: " ^ s
  | Uunexpected s -> "unexpected: " ^ s
  | Ulaunch s -> "launch: " ^ s
  | Unamed s -> "named: " ^ s
  | Uremote (filename, pageno) ->
      Printf.sprintf "%s: page %d" filename (pageno+1)
  | Uremotedest (filename, destname) ->
      Printf.sprintf "%s: destination %S" filename destname
  | Uannotation (opaque, slinkindex) ->
     "annotation: " ^ getannotcontents opaque slinkindex
;;

let updateunder x y =
  match getunder x y with
  | Unone -> Wsi.setcursor Wsi.CURSOR_INHERIT
  | Ulinkuri uri ->
      if conf.underinfo then showtext 'u' ("ri: " ^ uri);
      Wsi.setcursor Wsi.CURSOR_INFO
  | Ulinkgoto (pageno, _) ->
      if conf.underinfo
      then showtext 'p' ("age: " ^ string_of_int (pageno+1));
      Wsi.setcursor Wsi.CURSOR_INFO
  | Utext s ->
      if conf.underinfo then showtext 'f' ("ont: " ^ s);
      Wsi.setcursor Wsi.CURSOR_TEXT
  | Uunexpected s ->
      if conf.underinfo then showtext 'u' ("nexpected: " ^ s);
      Wsi.setcursor Wsi.CURSOR_INHERIT
  | Ulaunch s ->
      if conf.underinfo then showtext 'l' ("aunch: " ^ s);
      Wsi.setcursor Wsi.CURSOR_INHERIT
  | Unamed s ->
      if conf.underinfo then showtext 'n' ("amed: " ^ s);
      Wsi.setcursor Wsi.CURSOR_INHERIT
  | Uremote (filename, pageno) ->
      if conf.underinfo then showtext 'r'
        (Printf.sprintf "emote: %s (%d)" filename (pageno+1));
      Wsi.setcursor Wsi.CURSOR_INFO
  | Uremotedest (filename, destname) ->
      if conf.underinfo then showtext 'r'
        (Printf.sprintf "emote destination: %s (%S)" filename destname);
      Wsi.setcursor Wsi.CURSOR_INFO
  | Uannotation _ ->
      if conf.underinfo then showtext 'a' "nnotation";
      Wsi.setcursor Wsi.CURSOR_INFO
;;

let showlinktype under =
  if conf.underinfo && under != Unone
  then showtext ' ' @@ undertext under
;;

let intentry_with_suffix text key =
  let text =
    if key >= 32 && key < 127
    then
      let c = Char.chr key in
      match c with
      | '0' .. '9' ->
         addchar text c

      | 'k' | 'm' | 'g' | 'K' | 'M' | 'G' ->
         addchar text @@ asciilower c
      | _ ->
         state.text <- Printf.sprintf "invalid key (%d, `%c')" key c;
         text
    else (
      state.text <- Printf.sprintf "invalid key %d" key;
      text
    )
  in
  TEcont text
;;

let wcmd fmt =
  let b = Buffer.create 16 in
  Buffer.add_string b "llll";
  Printf.kbprintf
    (fun b ->
      let b = Buffer.to_bytes b in
      wcmd state.ss b @@ Bytes.length b
    ) b fmt
;;

let nogeomcmds cmds =
  match cmds with
  | s, [] -> emptystr s
  | _ -> false
;;

let layoutN ((columns, coverA, coverB), b) x y sw sh =
  let rec fold accu n =
    if n = Array.length b
    then accu
    else
      let pdimno, dx, vy, (_, w, h, xoff) = b.(n) in
      if (float vy -. y) > float sh &&
        (n = coverA - 1
            || n = state.pagecount - coverB
            || (n - coverA) mod columns = columns - 1)
      then accu
      else
        let accu =
          if float vy +. float h > y
          then
            let pagey = max 0 (truncate y - vy) in
            let pagedispy = if pagey > 0 then 0 else vy - truncate y in
            let pagedispx, pagex =
              let pdx =
                if n = coverA - 1 || n = state.pagecount - coverB
                then x + (sw - w) / 2
                else dx + xoff + x
              in
              if pdx < 0
              then 0, -pdx
              else pdx, 0
            in
            let pagevw =
              let vw = sw - pagedispx in
              let pw = w - pagex in
              min vw pw
            in
            let pagevh = min (h - pagey) (sh - pagedispy) in
            if pagevw > 0 && pagevh > 0
            then
              let e =
                { pageno = n
                ; pagedimno = pdimno
                ; pagew = w
                ; pageh = h
                ; pagex = pagex
                ; pagey = pagey
                ; pagevw = pagevw
                ; pagevh = pagevh
                ; pagedispx = pagedispx
                ; pagedispy = pagedispy
                ; pagecol = 0
                }
              in
              e :: accu
            else
              accu
          else
            accu
        in
        fold accu (n+1)
  in
  if Array.length b = 0
  then []
  else List.rev (fold [] (page_of_y y))
;;

let layoutS (columns, b) x y sw sh =
  let rec fold accu n =
    if n = Array.length b
    then accu
    else
      let pdimno, px, vy, (_, pagew, pageh, xoff) = b.(n) in
      if (vy - y) > sh
      then accu
      else
        let accu =
          if vy + pageh > y
          then
            let x = xoff + x in
            let pagey = max 0 (y - vy) in
            let pagedispy = if pagey > 0 then 0 else vy - y in
            let pagedispx, pagex =
              if px = 0
              then (
                if x < 0
                then 0, -x
                else x, 0
              )
              else (
                let px = px - x in
                if px < 0
                then -px, 0
                else 0, px
              )
            in
            let pagecolw = pagew/columns in
            let pagedispx =
              if pagecolw < sw
              then pagedispx + ((sw - pagecolw) / 2)
              else pagedispx
            in
            let pagevw =
              let vw = sw - pagedispx in
              let pw = pagew - pagex in
              min vw pw
            in
            let pagevw = min pagevw pagecolw in
            let pagevh = min (pageh - pagey) (sh - pagedispy) in
            if pagevw > 0 && pagevh > 0
            then
              let e =
                { pageno = n/columns
                ; pagedimno = pdimno
                ; pagew = pagew
                ; pageh = pageh
                ; pagex = pagex
                ; pagey = pagey
                ; pagevw = pagevw
                ; pagevh = pagevh
                ; pagedispx = pagedispx
                ; pagedispy = pagedispy
                ; pagecol = n mod columns
                }
              in
              e :: accu
            else
              accu
          else
            accu
        in
        fold accu (n+1)
  in
  List.rev (fold [] 0)
;;

let layout x y sw sh =
  if nogeomcmds state.geomcmds
  then
    match conf.columns with
    | Csingle b -> layoutN ((1, 0, 0), b) x y sw sh
    | Cmulti c -> layoutN c x y sw sh
    | Csplit s -> layoutS s x (truncate y) sw sh
  else []
;;

let clamp incr =
  let y = state.y +. float incr in
  let y = max 0.0 y in
  let y = min y (state.maxy -.
                   float (if conf.maxhfit then state.winh else 0)) in
  y;
;;

let itertiles l f =
  let tilex = l.pagex mod conf.tilew in
  let tiley = l.pagey mod conf.tileh in

  let col = l.pagex / conf.tilew in
  let row = l.pagey / conf.tileh in

  let rec rowloop row y0 dispy h =
    if h = 0
    then ()
    else (
      let dh = conf.tileh - y0 in
      let dh = min h dh in
      let rec colloop col x0 dispx w =
        if w = 0
        then ()
        else (
          let dw = conf.tilew - x0 in
          let dw = min w dw in
          f col row dispx dispy x0 y0 dw dh;
          colloop (col+1) 0 (dispx+dw) (w-dw)
        )
      in
      colloop col tilex l.pagedispx l.pagevw;
      rowloop (row+1) 0 (dispy+dh) (h-dh)
    )
  in
  if l.pagevw > 0 && l.pagevh > 0
  then rowloop row tiley l.pagedispy l.pagevh;
;;

let gettileopaque l col row =
  let key =
    l.pageno, state.gen, conf.colorspace, conf.angle, l.pagew, l.pageh, col, row
  in
  try Some (Hashtbl.find state.tilemap key)
  with Not_found -> None
;;

let puttileopaque l col row gen colorspace angle opaque size elapsed =
  let key = l.pageno, gen, colorspace, angle, l.pagew, l.pageh, col, row in
  Hashtbl.add state.tilemap key (opaque, size, elapsed)
;;

let filledrect2 x0 y0 x1 y1 x2 y2 x3 y3 =
  Raw.sets_float state.vraw ~pos:0 [| x0; y0; x1; y1; x2; y2; x3; y3 |];
  GlArray.vertex `two state.vraw;
  GlArray.draw_arrays `triangle_strip ~first:0 ~count:4;
;;

let filledrect1 x0 y0 x1 y1 = filledrect2 x0 y0 x0 y1 x1 y0 x1 y1;;

let filledrect x0 y0 x1 y1 =
  GlArray.disable `texture_coord;
  filledrect1 x0 y0 x1 y1;
  GlArray.enable `texture_coord;
;;

let linerect x0 y0 x1 y1 =
  GlArray.disable `texture_coord;
  Raw.sets_float state.vraw ~pos:0 [| x0; y0; x0; y1; x1; y1; x1; y0 |];
  GlArray.vertex `two state.vraw;
  GlArray.draw_arrays `line_loop ~first:0 ~count:4;
  GlArray.enable `texture_coord;
;;

let drawtiles l color =
  GlDraw.color color;
  begintiles ();
  let f col row x y tilex tiley w h =
    match gettileopaque l col row with
    | Some (opaque, _, t) ->
        let params = x, y, w, h, tilex, tiley in
        if conf.invert
        then GlTex.env (`mode `blend);
        drawtile params opaque;
        if conf.invert
        then GlTex.env (`mode `modulate);
        if conf.debug
        then (
          endtiles ();
          let s = Printf.sprintf
            "%d[%d,%d] %f sec"
            l.pageno col row t
          in
          let w = measurestr fstate.fontsize s in
          GlDraw.color (0.0, 0.0, 0.0);
          filledrect (float (x-2))
            (float (y-2))
            (float (x+2) +. w)
            (float (y + fstate.fontsize + 2));
          GlDraw.color color;
          drawstring fstate.fontsize x (y + fstate.fontsize - 1) s;
          begintiles ();
        );

    | None ->
        endtiles ();
        let w =
          let lw = state.winw - x in
          min lw w
        and h =
          let lh = state.winh - y in
          min lh h
        in
        if conf.invert
        then GlTex.env (`mode `blend);
        begin match state.checkerstexid with
        | Some id ->
            Gl.enable `texture_2d;
            GlTex.bind_texture ~target:`texture_2d id;
            let x0 = float x
            and y0 = float y
            and x1 = float (x+w)
            and y1 = float (y+h) in

            let tw = float w /. 16.0
            and th = float h /. 16.0 in
            let tx0 = float tilex /. 16.0
            and ty0 = float tiley /. 16.0 in
            let tx1 = tx0 +. tw
            and ty1 = ty0 +. th in
            Raw.sets_float state.vraw ~pos:0
              [| x0; y0; x0; y1; x1; y0; x1; y1 |];
            Raw.sets_float state.traw ~pos:0
              [| tx0; ty0; tx0; ty1; tx1; ty0; tx1; ty1 |];
            GlArray.vertex `two state.vraw;
            GlArray.tex_coord `two state.traw;
            GlArray.draw_arrays `triangle_strip ~first:0 ~count:4;
            Gl.disable `texture_2d;

        | None ->
            GlDraw.color (1.0, 1.0, 1.0);
            filledrect (float x) (float y) (float (x+w)) (float (y+h));
        end;
        if conf.invert
        then GlTex.env (`mode `modulate);
        if w > 128 && h > fstate.fontsize + 10
        then (
          let c = if conf.invert then 1.0 else 0.0 in
          GlDraw.color (c, c, c);
          let c, r =
            if conf.verbose
            then (col*conf.tilew, row*conf.tileh)
            else col, row
          in
          drawstring2 fstate.fontsize x y "Loading %d [%d,%d]" l.pageno c r;
        );
        GlDraw.color color;
        begintiles ();
  in
  itertiles l f;
  endtiles ();
;;

let pagevisible layout n = List.exists (fun l -> l.pageno = n) layout;;

let tilevisible1 l x y =
  let ax0 = l.pagex
  and ax1 = l.pagex + l.pagevw
  and ay0 = l.pagey
  and ay1 = l.pagey + l.pagevh in

  let bx0 = x
  and by0 = y in
  let bx1 = min (bx0 + conf.tilew) l.pagew
  and by1 = min (by0 + conf.tileh) l.pageh in

  let rx0 = max ax0 bx0
  and ry0 = max ay0 by0
  and rx1 = min ax1 bx1
  and ry1 = min ay1 by1 in

  let nonemptyintersection = rx1 > rx0 && ry1 > ry0 in
  nonemptyintersection
;;

let tilevisible layout n x y =
  let rec findpageinlayout m = function
    | l :: rest when l.pageno = n ->
        tilevisible1 l x y || (
          match conf.columns with
          | Csplit (c, _) when c > m -> findpageinlayout (m+1) rest
          | Csplit _
          | Csingle _
          | Cmulti _ -> false
        )
    | _ :: rest -> findpageinlayout 0 rest
    | [] -> false
  in
  findpageinlayout 0 layout;
;;

let tileready l x y =
  tilevisible1 l x y &&
    gettileopaque l (x/conf.tilew) (y/conf.tileh) != None
;;

let tilepage n p layout =
  let rec loop = function
    | l :: rest ->
        if l.pageno = n
        then
          let f col row _ _ _ _ _ _ =
            if state.currently = Idle
            then
              match gettileopaque l col row with
              | Some _ -> ()
              | None ->
                  let x = col*conf.tilew
                  and y = row*conf.tileh in
                  let w =
                    let w = l.pagew - x in
                    min w conf.tilew
                  in
                  let h =
                    let h = l.pageh - y in
                    min h conf.tileh
                  in
                  let pbo =
                    if conf.usepbo
                    then getpbo w h conf.colorspace
                    else ~< "0"
                  in
                  wcmd "tile %s %d %d %d %d %s"
                    (~> p) x y w h (~> pbo);
                  state.currently <-
                    Tiling (
                      l, p, conf.colorspace, conf.angle,
                      state.gen, col, row, conf.tilew, conf.tileh
                    );
          in
          itertiles l f;
        else
          loop rest

    | [] -> ()
  in
  if nogeomcmds state.geomcmds
  then loop layout;
;;

let preloadlayout x y sw sh =
  let y = if y < sh then 0 else y - sh in
  let x = min 0 (x + sw) in
  let h = sh*3 in
  let w = sw*3 in
  layout x (float y) w h;
;;

let load pages =
  let rec loop pages =
    if state.currently != Idle
    then ()
    else
      match pages with
      | l :: rest ->
          begin match getopaque l.pageno with
          | None ->
              wcmd "page %d %d" l.pageno l.pagedimno;
              state.currently <- Loading (l, state.gen);
          | Some opaque ->
              tilepage l.pageno opaque pages;
              loop rest
          end;
      | _ -> ()
  in
  if nogeomcmds state.geomcmds
  then loop pages
;;

let preload pages =
  load pages;
  if conf.preload && state.currently = Idle
  then load (preloadlayout (truncate state.x)
                           (truncate state.y) state.winw state.winh);
;;

let layoutready layout =
  let rec fold all ls =
    all && match ls with
    | l :: rest ->
        let seen = ref false in
        let allvisible = ref true in
        let foo col row _ _ _ _ _ _ =
          seen := true;
          allvisible := !allvisible &&
            begin match gettileopaque l col row with
            | Some _ -> true
            | None -> false
            end
        in
        itertiles l foo;
        fold (!seen && !allvisible) rest
    | [] -> true
  in
  let alltilesvisible = fold true layout in
  alltilesvisible;
;;

let gotoxy x y =
  let y = bound y 0.0 state.maxy in
  let y, layout, proceed =
    match conf.maxwait with
    | Some time when state.ghyll == noghyll ->
        begin match state.throttle with
        | None ->
            let layout = layout (truncate x) y state.winw state.winh in
            let ready = layoutready layout in
            if not ready
            then (
              load layout;
              state.throttle <- Some (layout, y, now ());
            )
            else G.postRedisplay "gotoxy showall (None)";
            y, layout, ready
        | Some (_, _, started) ->
            let dt = now () -. started in
            if dt > time
            then (
              state.throttle <- None;
              let layout = layout (truncate x) y state.winw state.winh in
              load layout;
              G.postRedisplay "maxwait";
              y, layout, true
            )
            else -1., [], false
        end

    | _ ->
        let layout = layout (truncate x) y state.winw state.winh in
        if not !wtmode || layoutready layout
        then G.postRedisplay "gotoxy ready";
        y, layout, true
  in
  if proceed
  then (
    state.x <- x;
    state.y <- y;
    state.layout <- layout;
    begin match state.mode with
    | LinkNav ln ->
        begin match ln with
        | Ltexact (pageno, linkno) ->
            let rec loop = function
              | [] ->
                  state.mode <- LinkNav (Ltgendir 0)
              | l :: _ when l.pageno = pageno ->
                  begin match getopaque pageno with
                  | None -> state.mode <- LinkNav (Ltnotready (pageno, 0))
                  | Some opaque ->
                      let x0, y0, x1, y1 = getlinkrect opaque linkno in
                      if not (x0 >= l.pagex && x1 <= l.pagex + l.pagevw
                                && y0 >= l.pagey && y1 <= l.pagey + l.pagevh)
                      then state.mode <- LinkNav (Ltgendir 0)
                  end
              | _ :: rest -> loop rest
            in
            loop layout
        | Ltnotready _ | Ltgendir _ -> ()
        end
    | Birdseye _
    | Textentry _
    | View -> ()
    end;
    begin match state.mode with
    | Birdseye (conf, leftx, pageno, hooverpageno, anchor) ->
        if not (pagevisible layout pageno)
        then (
          match state.layout with
          | [] -> ()
          | l :: _ ->
              state.mode <- Birdseye (
                conf, leftx, l.pageno, hooverpageno, anchor
              )
        );
    | LinkNav lt ->
        begin match lt with
        | Ltnotready (_, dir)
        | Ltgendir dir ->
            let linknav =
              let rec loop = function
                | [] -> lt
                | l :: rest ->
                    match getopaque l.pageno with
                    | None -> Ltnotready (l.pageno, dir)
                    | Some opaque ->
                        let link =
                          let ld =
                            if dir = 0
                            then LDfirstvisible (l.pagex, l.pagey, dir)
                            else (
                              if dir > 0 then LDfirst else LDlast
                             )
                          in
                          findlink opaque ld
                        in
                        match link with
                        | Lnotfound -> loop rest
                        | Lfound n ->
                            showlinktype (getlink opaque n);
                            Ltexact (l.pageno, n)
              in
              loop state.layout
            in
            state.mode <- LinkNav linknav
        | Ltexact _ -> ()
        end
    | Textentry _
    | View -> ()
    end;
    preload layout;
  );
  state.ghyll <- noghyll;
  if conf.updatecurs
  then (
    let mx, my = state.mpos in
    updateunder mx my;
  );
;;

let conttiling pageno opaque =
  tilepage pageno opaque
           (if conf.preload
            then preloadlayout (truncate state.x)
                               (truncate state.y) state.winw state.winh
            else state.layout)
;;

let gotoxy_and_clear_text x y =
  if not conf.verbose then state.text <- E.s;
  gotoxy x y;
;;

let getanchory (n, top, dtop) =
  let y, h = getpageyh n in
  if conf.presentation
  then
    let ips = calcips h in
    y +. top*.float h -. dtop*.float ips +. float ips;
  else
    y +. top*.float h -. dtop*.float conf.interpagespace
;;

let gotoanchor anchor =
  gotoxy state.x (getanchory anchor);
;;

let addnav () =
  cbput state.hists.nav (getanchor ());
;;

let getnav dir =
  let anchor = cbgetc state.hists.nav dir in
  getanchory anchor;
;;

let gotoghyll1 single y =
  let scroll f n a b =
    (* http://devmaster.net/forums/topic/9796-ease-in-ease-out-algorithm/ *)
    let snake f a b =
      let s x = 3.0*.x**2.0 -. 2.0*.x**3.0 in
      if f < a
      then s (float f /. float a)
      else (
        if f > b
        then 1.0 -. s ((float (f-b) /. float (n-b)))
        else 1.0
      );
    in
    snake f a b
  and summa n a b =
    let ins = float a *. 0.5
    and outs = float (n-b) *. 0.5 in
    let ones = b - a in
    ins +. outs +. float ones
  in
  let rec set nab y sy =
    let (_N, _A, _B), y =
      if single
      then
        let scl = if y > sy then 2 else -2 in
        let _N, _, _ = nab in
        (_N,0,_N), y+conf.scrollstep*scl
      else nab,y in
    let sum = summa _N _A _B in
    let dy = float (y - sy) in
    state.ghyll <- (
      let rec gf n y1 o =
        if n >= _N
        then state.ghyll <- noghyll
        else
          let go n =
            let s = scroll n _N _A _B in
            let y1 = y1 +. ((s *. dy) /. sum) in
            gotoxy_and_clear_text state.x y1;
            state.ghyll <- gf (n+1) y1;
          in
          match o with
          | None -> go n
          | Some y' when single -> set nab y' (truncate state.y)
          | Some y' -> set (_N/2, 1, 1) y' (truncate state.y)
      in
      gf 0 state.y
    )
  in
  match conf.ghyllscroll with
  | Some nab when not conf.presentation ->
      if state.ghyll == noghyll
      then set nab y (truncate state.y)
      else state.ghyll (Some y)
  | _ ->
      gotoxy_and_clear_text state.x (float y)
;;

let gotoghyll = gotoghyll1 false;;

let gotopage n top =
  let y, h = getpageyh n in
  let y = y +. (top *. float h) in
  gotoghyll (truncate y)
;;

let gotopage1 n top =
  let y = getpagey n in
  let y = truncate y + top in
  gotoghyll y
;;

let invalidate s f =
  state.redisplay <- false;
  state.layout <- [];
  state.pdims <- [];
  state.rects <- [];
  state.rects1 <- [];
  match state.geomcmds with
  | ps, [] when emptystr ps ->
      f ();
      state.geomcmds <- s, [];

  | ps, [] ->
      state.geomcmds <- ps, [s, f];

  | ps, (s', _) :: rest when s' = s ->
      state.geomcmds <- ps, ((s, f) :: rest);

  | ps, cmds ->
      state.geomcmds <- ps, ((s, f) :: cmds);
;;

let flushpages () =
  Hashtbl.iter (fun _ opaque ->
    wcmd "freepage %s" (~> opaque);
  ) state.pagemap;
  Hashtbl.clear state.pagemap;
;;

let flushtiles () =
  if not (Queue.is_empty state.tilelru)
  then (
    Queue.iter (fun (k, p, s) ->
      wcmd "freetile %s" (~> p);
      state.memused <- state.memused - s;
      Hashtbl.remove state.tilemap k;
    ) state.tilelru;
    state.uioh#infochanged Memused;
    Queue.clear state.tilelru;
  );
  load state.layout;
;;

let stateh h =
  let h = truncate (float h*.conf.zoom) in
  let d = conf.interpagespace lsl (if conf.presentation then 1 else 0) in
  h - d
;;

let opendoc path password =
  state.path <- path;
  state.password <- password;
  state.gen <- state.gen + 1;
  state.docinfo <- [];
  state.outlines <- [||];

  flushpages ();
  setaalevel conf.aalevel;
  let titlepath =
    if emptystr state.origin
    then path
    else state.origin
  in
  Wsi.settitle ("llpp " ^ (mbtoutf8 (Filename.basename titlepath)));
  wcmd "open %d %d %s\000%s\000" (btod !wtmode) (btod !cxack) path password;
  invalidate "reqlayout"
    (fun () ->
      wcmd "reqlayout %d %d %d %s\000"
        conf.angle (FMTE.to_int conf.fitmodel)
        (stateh state.winh) state.nameddest
    )
;;

let reload () =
  state.anchor <- getanchor ();
  opendoc state.path state.password;
;;

let scalecolor c =
  let c = c *. conf.colorscale in
  (c, c, c);
;;

let scalecolor2 (r, g, b) =
  (r *. conf.colorscale, g *. conf.colorscale, b *. conf.colorscale);
;;

let docolumns columns =
  match columns with
  | Csingle _ ->
      let a = Array.make state.pagecount (-1, -1, -1, (-1, -1, -1, -1)) in
      let rec loop pageno pdimno pdim y ph pdims =
        if pageno = state.pagecount
        then ()
        else
          let pdimno, ((_, w, h, xoff) as pdim), pdims =
            match pdims with
            | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
                pdimno+1, pdim, rest
            | _ ->
                pdimno, pdim, pdims
          in
          let x = max 0 (((state.winw - w) / 2) - xoff) in
          let y = y +
            (if conf.presentation
            then (if pageno = 0 then calcips h else calcips ph + calcips h)
              else (if pageno = 0 then 0 else conf.interpagespace)
            )
          in
          a.(pageno) <- (pdimno, x, y, pdim);
          loop (pageno+1) pdimno pdim (y + h) h pdims
      in
      loop 0 ~-1 (-1,-1,-1,-1) 0 0 state.pdims;
      conf.columns <- Csingle a;

  | Cmulti ((columns, coverA, coverB), _) ->
      let a = Array.make state.pagecount (-1, -1, -1, (-1, -1, -1, -1)) in
      let rec loop pageno pdimno pdim x y rowh pdims =
        let rec fixrow m = if m = pageno then () else
            let (pdimno, x, y, ((_, _, h, _) as pdim)) = a.(m) in
            if h < rowh
            then (
              let y = y + (rowh - h) / 2 in
              a.(m) <- (pdimno, x, y, pdim);
            );
            fixrow (m+1)
        in
        if pageno = state.pagecount
        then fixrow (((pageno - 1) / columns) * columns)
        else
          let pdimno, ((_, w, h, xoff) as pdim), pdims =
            match pdims with
            | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
                pdimno+1, pdim, rest
            | _ ->
                pdimno, pdim, pdims
          in
          let x, y, rowh' =
            if pageno = coverA - 1 || pageno = state.pagecount - coverB
            then (
              let x = (state.winw - w) / 2 in
              let ips =
                if conf.presentation then calcips h else conf.interpagespace in
              x, y + ips + rowh, h
            )
            else (
              if (pageno - coverA) mod columns = 0
              then (
                let x = max 0 (state.winw - state.w) / 2 in
                let y =
                  if conf.presentation
                  then
                    let ips = calcips h in
                    y + (if pageno = 0 then 0 else calcips rowh + ips)
                  else
                    y + (if pageno = 0 then 0 else conf.interpagespace)
                in
                x, y + rowh, h
              )
              else x, y, max rowh h
            )
          in
          let y =
            if pageno > 1 && (pageno - coverA) mod columns = 0
            then (
              let y =
                if pageno = columns && conf.presentation
                then (
                  let ips = calcips rowh in
                  for i = 0 to pred columns
                  do
                    let (pdimno, x, y, pdim) = a.(i) in
                    a.(i) <- (pdimno, x, y+ips, pdim)
                  done;
                  y+ips;
                )
                else y
              in
              fixrow (pageno - columns);
              y
            )
            else y
          in
          a.(pageno) <- (pdimno, x, y, pdim);
          let x = x + w + xoff*2 + conf.interpagespace in
          loop (pageno+1) pdimno pdim x y rowh' pdims
      in
      loop 0 ~-1 (-1,-1,-1,-1) 0 0 0 state.pdims;
      conf.columns <- Cmulti ((columns, coverA, coverB), a);

  | Csplit (c, _) ->
      let a = Array.make (state.pagecount*c) (-1, -1, -1, (-1, -1, -1, -1)) in
      let rec loop pageno pdimno pdim y pdims =
        if pageno = state.pagecount
        then ()
        else
          let pdimno, ((_, w, h, _) as pdim), pdims =
            match pdims with
            | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
                pdimno+1, pdim, rest
            | _ ->
                pdimno, pdim, pdims
          in
          let cw = w / c in
          let rec loop1 n x y =
            if n = c then y else (
              a.(pageno*c + n) <- (pdimno, x, y, pdim);
              loop1 (n+1) (x+cw) (y + h + conf.interpagespace)
            )
          in
          let y = loop1 0 0 y in
          loop (pageno+1) pdimno pdim y pdims
      in
      loop 0 ~-1 (-1,-1,-1,-1) 0 state.pdims;
      conf.columns <- Csplit (c, a);
;;

let represent () =
  docolumns conf.columns;
  state.maxy <- calcheight ();
  if state.reprf == noreprf
  then (
    match state.mode with
    | Birdseye (_, _, pageno, _, _) ->
        let y, h = getpageyh pageno in
        let top = (state.winh - h) / 2 in
        gotoxy state.x (max 0.0 (y -. float top))
    | Textentry _
    | View
    | LinkNav _ ->
       let y = getanchory state.anchor in
       let y = min y (state.maxy -. float state.winh) in
       gotoxy state.x y;
  )
  else (
    state.reprf ();
    state.reprf <- noreprf;
  );
;;

let reshape ?(firsttime=false) w h =
  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
  if not firsttime && nogeomcmds state.geomcmds
  then state.anchor <- getanchor ();

  state.winw <- w;
  let w = truncate (float w *. conf.zoom) in
  let w = max w 2 in
  state.winh <- h;
  setfontsize fstate.fontsize;
  GlMat.mode `modelview;
  GlMat.load_identity ();

  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.rotate ~x:1.0 ~angle:180.0 ();
  GlMat.translate ~x:~-.1.0 ~y:~-.1.0 ();
  GlMat.scale3 (2.0 /. float state.winw, 2.0 /. float state.winh, 1.0);

  let relx =
    if conf.zoom <= 1.0
    then 0.0
    else state.x /. float state.w
  in
  invalidate "geometry"
    (fun () ->
      state.w <- w;
      if not firsttime
      then state.x <- relx *. float w;
      let w =
        match conf.columns with
        | Csingle _ -> w
        | Cmulti ((c, _, _), _) -> (w - (c-1)*conf.interpagespace) / c
        | Csplit (c, _) -> w * c
      in
      wcmd "geometry %d %d %d"
        w (stateh h) (FMTE.to_int conf.fitmodel)
    );
;;

let enttext () =
  let len = String.length state.text in
  let x0 = if conf.leftscroll then vscrollw () else 0 in
  let drawstring s =
    let hscrollh =
      match state.mode with
      | Textentry _ | View | LinkNav _ ->
          let h, _, _ = state.uioh#scrollpw in
          h
      | Birdseye _ -> 0
    in
    let rect x w =
      filledrect x (float (state.winh - (fstate.fontsize + 4) - hscrollh))
        (x+.w) (float (state.winh - hscrollh))
    in

    let w = float (state.winw - 1 - vscrollw ()) in
    if state.progress >= 0.0 && state.progress < 1.0
    then (
      GlDraw.color (0.3, 0.3, 0.3);
      let w1 = w *. state.progress in
      rect (float x0) w1;
      GlDraw.color (0.0, 0.0, 0.0);
      rect (float x0+.w1) (float x0+.w-.w1)
    )
    else (
      GlDraw.color (0.0, 0.0, 0.0);
      rect (float x0) w;
    );

    GlDraw.color (1.0, 1.0, 1.0);
    drawstring fstate.fontsize
      (if conf.leftscroll then x0 + 2 else x0 + if len > 0 then 8 else 2)
      (state.winh - hscrollh - 5) s;
  in
  let s =
    match state.mode with
    | Textentry ((prefix, text, _, _, _, _), _) ->
        let s =
          if len > 0
          then
            Printf.sprintf "%s%s_ [%s]" prefix text state.text
          else
            Printf.sprintf "%s%s_"  prefix text
        in
        s

    | Birdseye _
    | View
    | LinkNav _ -> state.text
  in
  let s =
    if state.newerrmsgs
    then (
      if not (istextentry state.mode) && state.uioh#eformsgs
      then
        let s1 = "(press 'e' to review error messasges)" in
        if nonemptystr s then s ^ " " ^ s1 else s1
      else s
    )
    else s
  in
  if nonemptystr s
  then drawstring s
;;

let gctiles () =
  let len = Queue.length state.tilelru in
  let layout = lazy (
    match state.throttle with
    | None ->
        if conf.preload
        then preloadlayout (truncate state.x)
                           (truncate state.y) state.winw state.winh
        else state.layout
    | Some (layout, _, _) ->
        layout
  ) in
  let rec loop qpos =
    if state.memused <= conf.memlimit
    then ()
    else (
      if qpos < len
      then
        let (k, p, s) as lruitem = Queue.pop state.tilelru in
        let n, gen, colorspace, angle, pagew, pageh, col, row = k in
        let (_, pw, ph, _) = getpagedim n in
        if
          gen = state.gen
          && colorspace = conf.colorspace
          && angle = conf.angle
          && pagew = pw
          && pageh = ph
          && (
            let x = col*conf.tilew
            and y = row*conf.tileh in
            tilevisible (Lazy.force_val layout) n x y
          )
        then Queue.push lruitem state.tilelru
        else (
          freepbo p;
          wcmd "freetile %s" (~> p);
          state.memused <- state.memused - s;
          state.uioh#infochanged Memused;
          Hashtbl.remove state.tilemap k;
        );
        loop (qpos+1)
    )
  in
  loop 0
;;

let onpagerect pageno f =
  let b =
    match conf.columns with
    | Cmulti (_, b) -> b
    | Csingle b -> b
    | Csplit (_, b) -> b
  in
  if pageno >= 0 && pageno < Array.length b
  then
    let (_, _, _, (_, w, h, _)) = b.(pageno) in
    f w h
;;

let gotopagexy1 wtmode pageno x y =
  let _,w1,h1,leftx = getpagedim pageno in
  let top = y /. (float h1) in
  let left = x /. (float w1) in
  let py, w, h = getpageywh pageno in
  let wh = state.winh in
  let x = left *. (float w) in
  let x = leftx + truncate (state.x +. x) in
  let sx =
    if x < 0 || x >= state.winw
    then truncate state.x - x
    else truncate state.x
  in
  let pdy = truncate (top *. float h) in
  let y' = truncate py + pdy in
  let dy = y' - truncate state.y in
  let sy =
    if x != truncate state.x || not (dy > 0 && dy < wh)
    then (
      if conf.presentation
      then
        if abs (truncate py - y') > wh
        then y'
        else truncate py
      else y';
    )
    else truncate state.y
  in
  if truncate state.x != sx || truncate state.y != sy
  then (
    let x, y =
      if wtmode
      then (
        let ww = state.winw in
        let qx = sx / ww
        and qy = pdy / wh in
        let x = qx * ww
        and y = truncate py + qy * wh in
        let x = if -x + ww > w1 then -(w1-ww) else x
        and y' = if y + wh > truncate state.maxy
                 then truncate state.maxy - wh else y in
        let y =
          if conf.presentation
          then
            if abs (truncate py - y') > wh
            then y'
            else truncate py
          else y';
        in
        (x, y)
      )
      else (sx, sy)
    in
    gotoxy_and_clear_text (float x) (float y);
  )
  else gotoxy_and_clear_text state.x state.y;
;;

let gotopagexy wtmode pageno x y =
  match state.mode with
  | Birdseye _ -> gotopage pageno 0.0
  | Textentry _
  | View
  | LinkNav _ -> gotopagexy1 wtmode pageno x y
;;

let getpassword () =
  let passcmd = getenvwithdef "LLPP_ASKPASS" conf.passcmd in
  if emptystr passcmd
  then E.s
  else getcmdoutput
         (fun s ->
          impmsg "error getting password: %s" s;
          dolog "%s" s) passcmd;
;;

let pgoto opaque pageno x y =
  let pdimno = getpdimno pageno in
  let x, y = project opaque pageno pdimno x y in
  gotopagexy false pageno x y;
;;

let act cmds =
  (* dolog "%S" cmds; *)
  let spl = splitatspace cmds in
  let scan s fmt f =
    try Scanf.sscanf s fmt f
    with exn ->
      dolog "error processing '%S': %s" cmds @@ exntos exn;
      exit 1
  in
  let addoutline outline =
    match state.currently with
    | Outlining outlines ->
        state.currently <- Outlining (outline :: outlines)
    | Idle -> state.currently <- Outlining [outline]
    | Loading _
    | Tiling _ ->
        dolog "invalid outlining state";
        logcurrently state.currently
  in
  match spl with
  | "clear", "" ->
      state.uioh#infochanged Pdim;
      state.pdims <- [];

  | "clearrects", "" ->
      state.rects <- state.rects1;
      G.postRedisplay "clearrects";

  | "continue", args ->
      let n = scan args "%u" (fun n -> n) in
      state.pagecount <- n;
      begin match state.currently with
      | Outlining l ->
          state.currently <- Idle;
          state.outlines <- Array.of_list (List.rev l)
      | Idle
      | Loading _
      | Tiling _ -> ()
      end;

      let cur, cmds = state.geomcmds in
      if emptystr cur
      then failwith "umpossible";

      begin match List.rev cmds with
      | [] ->
          state.geomcmds <- E.s, [];
          state.throttle <- None;
          represent ();
      | (s, f) :: rest ->
          f ();
          state.geomcmds <- s, List.rev rest;
      end;
      if conf.maxwait = None && not !wtmode
      then G.postRedisplay "continue";

  | "msg", args ->
      showtext ' ' args

  | "vmsg", args ->
      if conf.verbose
      then showtext ' ' args

  | "emsg", args ->
      Buffer.add_string state.errmsgs args;
      state.newerrmsgs <- true;
      G.postRedisplay "error message"

  | "progress", args ->
      let progress, text =
        scan args "%f %n"
          (fun f pos ->
            f, String.sub args pos (String.length args - pos))
      in
      state.text <- text;
      state.progress <- progress;
      G.postRedisplay "progress"

  | "firstmatch", args ->
      let pageno, c, x0, y0, x1, y1, x2, y2, x3, y3 =
        scan args "%u %d %f %f %f %f %f %f %f %f"
          (fun p c x0 y0 x1 y1 x2 y2 x3 y3 ->
            (p, c, x0, y0, x1, y1, x2, y2, x3, y3))
      in
      let y = truncate ((getpagey pageno) +. y0) in
      let x =
        if conf.zoom > 1.0
        then state.winw/2
        else truncate state.x
      in
      addnav ();
      gotoxy (float x) (float y);
      let color = (0.0, 0.0, 1.0 /. float c, 0.5) in
      state.rects1 <- [pageno, color, (x0, y0, x1, y1, x2, y2, x3, y3)]

  | "match", args ->
      let pageno, c, x0, y0, x1, y1, x2, y2, x3, y3 =
        scan args "%u %d %f %f %f %f %f %f %f %f"
          (fun p c x0 y0 x1 y1 x2 y2 x3 y3 ->
            (p, c, x0, y0, x1, y1, x2, y2, x3, y3))
      in
      let color = (0.0, 0.0, 1.0 /. float c, 0.5) in
      state.rects1 <-
        (pageno, color, (x0, y0, x1, y1, x2, y2, x3, y3)) :: state.rects1

  | "page", args ->
      let pageopaques, t = scan args "%s %f" (fun p t -> p, t) in
      let pageopaque = ~< pageopaques in
      begin match state.currently with
      | Loading (l, gen) ->
          vlog "page %d took %f sec" l.pageno t;
          Hashtbl.replace state.pagemap (l.pageno, gen) pageopaque;
          begin match state.throttle with
          | None ->
              let preloadedpages =
                if conf.preload
                then preloadlayout
                       (truncate state.x)
                       (truncate state.y) state.winw state.winh
                else state.layout
              in
              let evict () =
                let set =
                  List.fold_left (fun s l -> IntSet.add l.pageno s)
                    IntSet.empty preloadedpages
                in
                let evictedpages =
                  Hashtbl.fold (fun ((pageno, _) as key) opaque accu ->
                    if not (IntSet.mem pageno set)
                    then (
                      wcmd "freepage %s" (~> opaque);
                      key :: accu
                    )
                    else accu
                  ) state.pagemap []
                in
                List.iter (Hashtbl.remove state.pagemap) evictedpages;
              in
              evict ();
              state.currently <- Idle;
              if gen = state.gen
              then (
                tilepage l.pageno pageopaque state.layout;
                load state.layout;
                load preloadedpages;
                let visible = pagevisible state.layout l.pageno in
                if visible
                then (
                  match state.mode with
                  | LinkNav (Ltnotready (pageno, dir)) ->
                      if pageno = l.pageno
                      then (
                        let link =
                          let ld =
                            if dir = 0
                            then LDfirstvisible (l.pagex, l.pagey, dir)
                            else (
                              if dir > 0 then LDfirst else LDlast
                             )
                          in
                          findlink pageopaque ld
                        in
                        match link with
                        | Lnotfound -> ()
                        | Lfound n ->
                            showlinktype (getlink pageopaque n);
                            state.mode <- LinkNav (Ltexact (l.pageno, n))
                       )
                  | LinkNav (Ltgendir _)
                  | LinkNav (Ltexact _)
                  | View
                  | Birdseye _
                  | Textentry _ -> ()
                 );

                if visible && layoutready state.layout
                then (
                  G.postRedisplay "page";
                 )
              )

          | Some (layout, _, _) ->
              state.currently <- Idle;
              tilepage l.pageno pageopaque layout;
              load state.layout
          end;

      | Idle
      | Tiling _
      | Outlining _ ->
          dolog "Inconsistent loading state";
          logcurrently state.currently;
          exit 1
      end

  | "tile" , args ->
      let (x, y, opaques, size, t) =
        scan args "%u %u %s %u %f"
          (fun x y p size t -> (x, y, p, size, t))
      in
      let opaque = ~< opaques in
      begin match state.currently with
      | Tiling (l, pageopaque, cs, angle, gen, col, row, tilew, tileh) ->
          vlog "tile %d [%d,%d] took %f sec" l.pageno col row t;

          unmappbo opaque;
          if tilew != conf.tilew || tileh != conf.tileh
          then (
            wcmd "freetile %s" (~> opaque);
            state.currently <- Idle;
            load state.layout;
          )
          else (
            puttileopaque l col row gen cs angle opaque size t;
            state.memused <- state.memused + size;
            state.uioh#infochanged Memused;
            gctiles ();
            Queue.push ((l.pageno, gen, cs, angle, l.pagew, l.pageh, col, row),
                       opaque, size) state.tilelru;

            let layout =
              match state.throttle with
              | None -> state.layout
              | Some (layout, _, _) -> layout
            in

            state.currently <- Idle;
            if   gen = state.gen
              && conf.colorspace = cs
              && conf.angle = angle
              && tilevisible layout l.pageno x y
            then conttiling l.pageno pageopaque;

            begin match state.throttle with
            | None ->
                preload state.layout;
                if   gen = state.gen
                  && conf.colorspace = cs
                  && conf.angle = angle
                  && tilevisible state.layout l.pageno x y
                  && (not !wtmode || layoutready state.layout)
                then G.postRedisplay "tile nothrottle";

            | Some (layout, y, _) ->
                let ready = layoutready layout in
                if ready
                then (
                  state.y <- y;
                  state.layout <- layout;
                  state.throttle <- None;
                  G.postRedisplay "throttle";
                )
                else load layout;
            end;
          );

      | Idle
      | Loading _
      | Outlining _ ->
          dolog "Inconsistent tiling state";
          logcurrently state.currently;
          exit 1
      end

  | "pdim", args ->
      let (n, w, h, _) as pdim =
        scan args "%u %u %u %u" (fun n w h x -> n, w, h, x)
      in
      let pdim =
        match conf.fitmodel with
        | FitWidth -> pdim
        | FitPage | FitProportional ->
            match conf.columns with
            | Csplit _ ->  (n, w, h, 0)
            | Csingle _ | Cmulti _ -> pdim
      in
      state.uioh#infochanged Pdim;
      state.pdims <- pdim :: state.pdims

  | "o", args ->
      let (l, n, t, h, pos) =
        scan args "%u %u %d %u %n"
          (fun l n t h pos -> l, n, t, h, pos)
      in
      let s = String.sub args pos (String.length args - pos) in
      addoutline (s, l, Oanchor (n, float t /. float h, 0.0))

  | "ou", args ->
      let (l, len, pos) = scan args "%u %u %n" (fun l len pos -> l, len, pos) in
      let s = String.sub args pos len in
      let pos2 = pos + len + 1 in
      let uri = String.sub args pos2 (String.length args - pos2) in
      addoutline (s, l, Ouri uri)

  | "on", args ->
      let (l, pos) = scan args "%u %n" (fun l pos -> l, pos) in
      let s = String.sub args pos (String.length args - pos) in
      addoutline (s, l, Onone)

  | "a", args ->
      let (n, l, t) =
        scan args "%u %d %d" (fun n l t -> n, l, t)
      in
      state.reprf <- (fun () -> gotopagexy !wtmode n (float l) (float t))

  | "info", args ->
      let pos = nindex args '\t' in
      if pos >= 0 && String.sub args 0 pos = "Title"
      then (
        let s = String.sub args (pos+1) @@ String.length args - pos - 1 in
        conf.title <- s;
        Wsi.settitle s;
      );
      state.docinfo <- (1, args) :: state.docinfo

  | "infoend", "" ->
      state.uioh#infochanged Docinfo;
      state.docinfo <- List.rev state.docinfo

  | "pass", args ->
     if args = "fail"
     then Wsi.settitle "Wrong password";
     let password = getpassword () in
     if emptystr password
     then error "document is password protected"
     else opendoc state.path password

  | _ ->
      error "unknown cmd `%S'" cmds
;;

let onhist cb =
  let rc = cb.rc in
  let action = function
    | HCprev  -> cbget cb ~-1
    | HCnext  -> cbget cb 1
    | HCfirst -> cbget cb ~-(cb.rc)
    | HClast  -> cbget cb (cb.len - 1 - cb.rc)
  and cancel () = cb.rc <- rc
  in (action, cancel)
;;

let search pattern forward =
  match conf.columns with
  | Csplit _ -> impmsg "searching does not work properly in split columns mode"
  | Csingle _
  | Cmulti _ ->
      if nonemptystr pattern
      then
        let pn, py =
          match state.layout with
          | [] -> 0, 0
          | l :: _ ->
              l.pageno, (l.pagey + if forward then 0 else 0*l.pagevh)
        in
        wcmd "search %d %d %d %d,%s\000"
          (btod conf.icase) pn py (btod forward) pattern;
;;

let intentry text key =
  let text =
    if key >= 32 && key < 127
    then
      let c = Char.chr key in
      match c with
      | '0' .. '9' -> addchar text c
      | _ ->
         state.text <- Printf.sprintf "invalid char (%d, `%c')" key c;
         text
    else (
      state.text <- Printf.sprintf "invalid key (%d)" key;
      text
    )
  in
  TEcont text
;;

let linknact f s =
  if nonemptystr s
  then (
    let n =
      let l = String.length s in
      let rec loop pos n = if pos = l then n else
          let m = Char.code s.[pos] - (if pos = 0 && l > 1 then 96 else 97) in
          loop (pos+1) (n*26 + m)
      in loop 0 0
    in
    let rec loop n = function
      | [] -> ()
      | l :: rest ->
          match getopaque l.pageno with
          | None -> loop n rest
          | Some opaque ->
              let m = getlinkcount opaque in
              if n < m
              then (
                let under = getlink opaque n in
                f under
              )
              else loop (n-m) rest
    in
    loop n state.layout;
  )
;;

let linknentry text key =
  if key >= 32 && key < 127
  then
    let text = addchar text (Char.chr key) in
    linknact (fun under -> state.text <- undertext ~nopath:true under) text;
    TEcont text
  else (
    state.text <- Printf.sprintf "invalid key %d" key;
    TEcont text
  )
;;

let textentry text key =
  if Wsi.isspecialkey key
  then TEcont text
  else TEcont (text ^ toutf8 key)
;;

let reqlayout angle fitmodel =
  match state.throttle with
  | None ->
      if nogeomcmds state.geomcmds
      then state.anchor <- getanchor ();
      conf.angle <- angle mod 360;
      if conf.angle != 0
      then (
        match state.mode with
        | LinkNav _ -> state.mode <- View
        | Birdseye _
        | Textentry _
        | View -> ()
      );
      conf.fitmodel <- fitmodel;
      invalidate "reqlayout"
        (fun () ->
          wcmd "reqlayout %d %d %d"
            conf.angle (FMTE.to_int conf.fitmodel) (stateh state.winh)
        );
  | _ -> ()
;;

let settrim trimmargins trimfuzz =
  if nogeomcmds state.geomcmds
  then state.anchor <- getanchor ();
  conf.trimmargins <- trimmargins;
  conf.trimfuzz <- trimfuzz;
  let x0, y0, x1, y1 = trimfuzz in
  invalidate "settrim"
    (fun () ->
      wcmd "settrim %d %d %d %d %d" (btod conf.trimmargins) x0 y0 x1 y1);
  flushpages ();
;;

let setzoom zoom =
  match state.throttle with
  | None ->
      let zoom = max 0.0001 zoom in
      if zoom <> conf.zoom
      then (
        state.prevzoom <- (conf.zoom, truncate state.x);
        conf.zoom <- zoom;
        reshape state.winw state.winh;
        state.text <- Printf.sprintf "zoom is now %-5.2f" (zoom *. 100.0);
      )

  | Some (layout, y, started) ->
      let time =
        match conf.maxwait with
        | None -> 0.0
        | Some t -> t
      in
      let dt = now () -. started in
      if dt > time
      then (
        state.y <- y;
        load layout;
      )
;;

let pivotzoom ?(vw=float state.winw)
              ?(vh=min (state.maxy-.state.y) (float state.winh))
              ?(x=vw/.2.) ?(y=vh/.2.) zoom =
  let w = float state.w /. zoom in
  let hw = w /. 2.0 in
  let ratio = vh /. vw in
  let hh = hw *. ratio in
  let x0 = x -. hw in
  let y0 = y -. hh in
  gotoxy (state.x -. x0) (state.y +. y0);
  setzoom zoom;
;;

let pivotzoom ?vw ?vh ?x ?y zoom =
  if nogeomcmds state.geomcmds then pivotzoom ?vw ?vh ?x ?y zoom
;;

let setcolumns mode columns coverA coverB =
  state.prevcolumns <- Some (conf.columns, conf.zoom);
  if columns < 0
  then (
    if isbirdseye mode
    then impmsg "split mode doesn't work in bird's eye"
    else (
      conf.columns <- Csplit (-columns, E.a);
      state.x <- 0.0;
      conf.zoom <- 1.0;
    );
  )
  else (
    if columns < 2
    then (
      conf.columns <- Csingle E.a;
      state.x <- 0.0;
      setzoom 1.0;
    )
    else (
      conf.columns <- Cmulti ((columns, coverA, coverB), E.a);
      conf.zoom <- 1.0;
    );
  );
  reshape state.winw state.winh;
;;

let resetmstate () =
  state.mstate <- Mnone;
  Wsi.setcursor Wsi.CURSOR_INHERIT;
;;

let enterbirdseye () =
  let zoom = float conf.thumbw /. float state.winw in
  let birdseyepageno =
    let cy = state.winh / 2 in
    let fold = function
      | [] -> 0
      | l :: rest ->
          let rec fold best = function
            | [] -> best.pageno
            | l :: rest ->
                let d = cy - (l.pagedispy + l.pagevh/2)
                and dbest = cy - (best.pagedispy + best.pagevh/2) in
                if abs d < abs dbest
                then fold l rest
                else best.pageno
          in fold l rest
    in
    fold state.layout
  in
  state.mode <- Birdseye (
                    { conf with zoom = conf.zoom }, truncate state.x,
                    birdseyepageno, -1, getanchor ()
  );
  resetmstate ();
  conf.zoom <- zoom;
  conf.presentation <- false;
  conf.interpagespace <- 10;
  conf.hlinks <- false;
  conf.fitmodel <- FitPage;
  state.x <- 0.0;
  conf.maxwait <- None;
  conf.columns <- (
    match conf.beyecolumns with
    | Some c ->
        conf.zoom <- 1.0;
        Cmulti ((c, 0, 0), E.a)
    | None -> Csingle E.a
  );
  if conf.verbose
  then
    state.text <- Printf.sprintf "birds eye mode on (zoom %3.1f%%)"
      (100.0*.zoom)
  else
    state.text <- E.s
  ;
  reshape state.winw state.winh;
;;

let leavebirdseye (c, leftx, pageno, _, anchor) goback =
  state.mode <- View;
  conf.zoom <- c.zoom;
  conf.presentation <- c.presentation;
  conf.interpagespace <- c.interpagespace;
  conf.maxwait <- c.maxwait;
  conf.hlinks <- c.hlinks;
  conf.fitmodel <- c.fitmodel;
  conf.beyecolumns <- (
    match conf.columns with
    | Cmulti ((c, _, _), _) -> Some c
    | Csingle _ -> None
    | Csplit _ -> failwith "leaving bird's eye split mode"
  );
  conf.columns <- (
    match c.columns with
    | Cmulti (c, _) -> Cmulti (c, E.a)
    | Csingle _ -> Csingle E.a
    | Csplit (c, _) -> Csplit (c, E.a)
  );
  if conf.verbose
  then
    state.text <- Printf.sprintf "birds eye mode off (zoom %3.1f%%)"
      (100.0*.conf.zoom)
  ;
  reshape state.winw state.winh;
  state.anchor <- if goback then anchor else (pageno, 0.0, 1.0);
  state.x <- float leftx;
;;

let togglebirdseye () =
  match state.mode with
  | Birdseye vals -> leavebirdseye vals true
  | View -> enterbirdseye ()
  | Textentry _
  | LinkNav _ -> ()
;;

let upbirdseye incr (conf, leftx, pageno, hooverpageno, anchor) =
  let pageno = max 0 (pageno - incr) in
  let rec loop = function
    | [] -> gotopage1 pageno 0
    | l :: _ when l.pageno = pageno ->
        if l.pagedispy >= 0 && l.pagey = 0
        then G.postRedisplay "upbirdseye"
        else gotopage1 pageno 0
    | _ :: rest -> loop rest
  in
  loop state.layout;
  state.text <- E.s;
  state.mode <- Birdseye (conf, leftx, pageno, hooverpageno, anchor)
;;

let downbirdseye incr (conf, leftx, pageno, hooverpageno, anchor) =
  let pageno = min (state.pagecount - 1) (pageno + incr) in
  state.mode <- Birdseye (conf, leftx, pageno, hooverpageno, anchor);
  let rec loop = function
    | [] ->
        let y, h = getpageyh pageno in
        let dy = truncate (y -. state.y)
                 - (state.winh - h - conf.interpagespace) in
        gotoxy state.x (clamp dy)
    | l :: _ when l.pageno = pageno ->
        if l.pagevh != l.pageh
        then gotoxy state.x (clamp (l.pageh - l.pagevh + conf.interpagespace))
        else G.postRedisplay "downbirdseye"
    | _ :: rest -> loop rest
  in
  loop state.layout;
  state.text <- E.s;
;;

let optentry mode _ key =
  let btos b = if b then "on" else "off" in
  if key >= 32 && key < 127
  then
    let c = Char.chr key in
    match c with
    | 's' ->
        let ondone s =
          try conf.scrollstep <- int_of_string s with exn ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s @@ exntos exn
        in
        TEswitch ("scroll step: ", E.s, None, intentry, ondone, true)

    | 'A' ->
        let ondone s =
          try
            conf.autoscrollstep <- boundastep state.winh (int_of_string s);
            if state.autoscroll <> None
            then state.autoscroll <- Some conf.autoscrollstep
          with exn ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s @@ exntos exn
        in
        TEswitch ("auto scroll step: ", E.s, None, intentry, ondone, true)

    | 'C' ->
        let ondone s =
          try
            let n, a, b = multicolumns_of_string s in
            setcolumns mode n a b;
          with exn ->
            state.text <- Printf.sprintf "bad columns `%s': %s" s @@ exntos exn
        in
        TEswitch ("columns: ", E.s, None, textentry, ondone, true)

    | 'Z' ->
        let ondone s =
          try
            let zoom = float (int_of_string s) /. 100.0 in
            pivotzoom zoom
          with exn ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s @@ exntos exn
        in
        TEswitch ("zoom: ", E.s, None, intentry, ondone, true)

    | 't' ->
        let ondone s =
          try
            conf.thumbw <- bound (int_of_string s) 2 4096;
            state.text <-
              Printf.sprintf "thumbnail width is set to %d" conf.thumbw;
            begin match mode with
            | Birdseye beye ->
                leavebirdseye beye false;
                enterbirdseye ();
            | Textentry _
            | View
            | LinkNav _ -> ();
            end
          with exn ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s @@ exntos exn
        in
        TEswitch ("thumbnail width: ", E.s, None, intentry, ondone, true)

    | 'R' ->
        let ondone s =
          match int_of_string s with
          | angle -> reqlayout angle conf.fitmodel
          | exception exn ->
             state.text <-
               Printf.sprintf "bad integer `%s': %s" s @@ exntos exn
        in
        TEswitch ("rotation: ", E.s, None, intentry, ondone, true)

    | 'i' ->
        conf.icase <- not conf.icase;
        TEdone ("case insensitive search " ^ (btos conf.icase))

    | 'p' ->
        conf.preload <- not conf.preload;
        gotoxy state.x state.y;
        TEdone ("preload " ^ (btos conf.preload))

    | 'v' ->
        conf.verbose <- not conf.verbose;
        TEdone ("verbose " ^ (btos conf.verbose))

    | 'd' ->
        conf.debug <- not conf.debug;
        TEdone ("debug " ^ (btos conf.debug))

    | 'h' ->
        conf.maxhfit <- not conf.maxhfit;
        state.maxy <- calcheight ();
        TEdone ("maxhfit " ^ (btos conf.maxhfit))

    | 'c' ->
        conf.crophack <- not conf.crophack;
        TEdone ("crophack " ^ btos conf.crophack)

    | 'a' ->
        let s =
          match conf.maxwait with
          | None ->
              conf.maxwait <- Some infinity;
              "always wait for page to complete"
          | Some _ ->
              conf.maxwait <- None;
              "show placeholder if page is not ready"
        in
        TEdone s

    | 'f' ->
        conf.underinfo <- not conf.underinfo;
        TEdone ("underinfo " ^ btos conf.underinfo)

    | 'P' ->
        conf.savebmarks <- not conf.savebmarks;
        TEdone ("persistent bookmarks " ^ btos conf.savebmarks)

    | 'S' ->
        let ondone s =
          try
            let pageno, py =
              match state.layout with
              | [] -> 0, 0
              | l :: _ ->
                  l.pageno, l.pagey
            in
            conf.interpagespace <- int_of_string s;
            docolumns conf.columns;
            state.maxy <- calcheight ();
            let y = getpagey pageno in
            gotoxy state.x (y +. float py)
          with exn ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s @@ exntos exn
        in
        TEswitch ("vertical margin: ", E.s, None, intentry, ondone, true)

    | 'l' ->
        let fm =
          match conf.fitmodel with
          | FitProportional -> FitWidth
          | FitWidth | FitPage -> FitProportional
        in
        reqlayout conf.angle fm;
        TEdone ("proportional display " ^ btos (fm == FitProportional))

    | 'T' ->
        settrim (not conf.trimmargins) conf.trimfuzz;
        TEdone ("trim margins " ^ btos conf.trimmargins)

    | 'I' ->
        conf.invert <- not conf.invert;
        TEdone ("invert colors " ^ btos conf.invert)

    | 'x' ->
        let ondone s =
          cbput state.hists.sel s;
          conf.selcmd <- s;
        in
        TEswitch ("selection command: ", E.s, Some (onhist state.hists.sel),
                 textentry, ondone, true)

    | 'M' ->
        if conf.pax == None
        then conf.pax <- Some (ref (0.0, 0, 0))
        else conf.pax <- None;
        TEdone ("PAX " ^ btos (conf.pax != None))

    | _ ->
        state.text <- Printf.sprintf "bad option %d `%c'" key c;
        TEstop
  else
    TEcont state.text
;;

class type lvsource = object
  method getitemcount : int
  method getitem : int -> (string * int)
  method hasaction : int -> bool
  method exit :
    uioh:uioh ->
    cancel:bool ->
    active:int ->
    first:int ->
    pan:int ->
    uioh option
  method getactive : int
  method getfirst : int
  method getpan : int
  method getminfo : (int * int) array
end;;

class virtual lvsourcebase = object
  val mutable m_active = 0
  val mutable m_first = 0
  val mutable m_pan = 0
  method getactive = m_active
  method getfirst = m_first
  method getpan = m_pan
  method getminfo : (int * int) array = E.a
end;;

let textentrykeyboard
    key _mask ((c, text, opthist, onkey, ondone, cancelonempty), onleave) =
  state.text <- E.s;
  let key = Wsi.keypadtodigitkey key in
  let enttext te =
    state.mode <- Textentry (te, onleave);
    enttext ();
    G.postRedisplay "textentrykeyboard enttext";
  in
  let histaction cmd =
    match opthist with
    | None -> ()
    | Some (action, _) ->
        state.mode <- Textentry (
          (c, action cmd, opthist, onkey, ondone, cancelonempty), onleave
        );
        G.postRedisplay "textentry histaction"
  in
  match key with
  | @backspace ->
      if emptystr text && cancelonempty
      then (
        onleave Cancel;
        G.postRedisplay "textentrykeyboard after cancel";
      )
      else
        let s = withoutlastutf8 text in
        enttext (c, s, opthist, onkey, ondone, cancelonempty)

  | @enter | @kpenter ->
      ondone text;
      onleave Confirm;
      G.postRedisplay "textentrykeyboard after confirm"

  | @up | @kpup -> histaction HCprev
  | @down | @kpdown -> histaction HCnext
  | @home | @kphome -> histaction HCfirst
  | @jend | @kpend -> histaction HClast

  | @escape ->
      if emptystr text
      then (
        begin match opthist with
        | None -> ()
        | Some (_, onhistcancel) -> onhistcancel ()
        end;
        onleave Cancel;
        state.text <- E.s;
        G.postRedisplay "textentrykeyboard after cancel2"
      )
      else (
        enttext (c, E.s, opthist, onkey, ondone, cancelonempty)
      )

  | @delete | @kpdelete -> ()

  | _ when key != 0 && not (Wsi.isspecialkey key) ->
      begin match onkey text key with
      | TEdone text ->
          ondone text;
          onleave Confirm;
          G.postRedisplay "textentrykeyboard after confirm2";

      | TEcont text ->
          enttext (c, text, opthist, onkey, ondone, cancelonempty);

      | TEstop ->
          onleave Cancel;
          G.postRedisplay "textentrykeyboard after cancel3"

      | TEswitch te ->
          state.mode <- Textentry (te, onleave);
          G.postRedisplay "textentrykeyboard switch";
      end;

  | _ ->
      vlog "unhandled key %s" (Wsi.keyname key)
;;

let firstof first active =
  if first > active || abs (first - active) > fstate.maxrows - 1
  then max 0 (active - (fstate.maxrows/2))
  else first
;;

let calcfirst first active =
  if active > first
  then
    let rows = active - first in
    if rows > fstate.maxrows then active - fstate.maxrows else first
  else active
;;

let scrollph y maxy =
  let sh = float (maxy + state.winh) /. float state.winh in
  let sh = float state.winh /. sh in
  let sh = max sh (float conf.scrollh) in

  let percent = float y /. float maxy in
  let position = (float state.winh -. sh) *. percent in

  let position =
    if position +. sh > float state.winh
    then float state.winh -. sh
    else position
  in
  position, sh;
;;

let adderrmsg src msg =
  Buffer.add_string state.errmsgs msg;
  state.newerrmsgs <- true;
  G.postRedisplay src
;;

let adderrfmt src fmt =
  Format.ksprintf (fun s -> adderrmsg src s) fmt;
;;

let coe s = (s :> uioh);;

class listview ~zebra ~helpmode ~(source:lvsource) ~trusted ~modehash =
object (self)
  val m_pan = source#getpan
  val m_first = source#getfirst
  val m_active = source#getactive
  val m_qsearch = E.s
  val m_prev_uioh = state.uioh

  method private elemunder y =
    if y < 0
    then None
    else
      let n = y / (fstate.fontsize+1) in
      if m_first + n < source#getitemcount
      then (
        if source#hasaction (m_first + n)
        then Some (m_first + n)
        else None
      )
      else None

  method display =
    Gl.enable `blend;
    GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
    GlDraw.color (0., 0., 0.) ~alpha:0.85;
    filledrect 0. 0. (float state.winw) (float state.winh);
    GlDraw.color (1., 1., 1.);
    Gl.enable `texture_2d;
    let fs = fstate.fontsize in
    let nfs = fs + 1 in
    let hw = state.winw/3 in
    let ww = fstate.wwidth in
    let tabw = 17.0*.ww in
    let itemcount = source#getitemcount in
    let minfo = source#getminfo in
    if conf.leftscroll
    then (
      GlMat.push ();
      GlMat.translate ~x:(float conf.scrollbw) ();
    );
    let x0 = 0.0 and x1 = float (state.winw - conf.scrollbw - 1) in
    let rec loop row =
      if (row - m_first) > fstate.maxrows
      then ()
      else (
        if row >= 0 && row < itemcount
        then (
          let (s, level) = source#getitem row in
          let y = (row - m_first) * nfs in
          let x = 5.0 +. (float (level + m_pan)) *. ww in
          if helpmode
          then GlDraw.color
              (let c = if row land 1 = 0 then 1.0 else 0.92 in (c,c,c));

          if row = m_active
          then (
            Gl.disable `texture_2d;
            let alpha = if source#hasaction row then 0.9 else 0.3 in
            GlDraw.color (1., 1., 1.) ~alpha;
            linerect (x0 +. 1.) (float (y + 1)) (x1) (float (y + fs + 3));
            Gl.enable `texture_2d;
          );
          let c =
            if zebra && row land 1 = 1
            then 0.8
            else 1.0
          in
          GlDraw.color (c,c,c);
          let drawtabularstring s =
            let drawstr x s =
              let x' = truncate (x0 +. x) in
              let pos = nindex s '\000' in
              if pos = -1
              then drawstring1 fs x' (y+nfs) s
              else
                let s1 = String.sub s 0 pos
                and s2 = String.sub s (pos+1) (String.length s - pos - 1) in
                let rec e s =
                  if emptystr s
                  then s
                  else
                    let s' = withoutlastutf8 s in
                    let s = s' ^ "@Uellipsis" in
                    let w = measurestr fs s in
                    if float x' +. w +. ww < float (hw + x')
                    then s
                    else e s'
                in
                let s1 =
                  if float x' +. ww +. measurestr fs s1 > float (hw + x')
                  then e s1
                  else s1
                in
                ignore (drawstring1 fs x' (y+nfs) s1);
                drawstring1 fs (hw + x') (y+nfs) s2
            in
            if trusted
            then
              let x = if helpmode && row > 0 then x +. ww else x in
              let tabpos = nindex s '\t' in
              if tabpos > 0
              then
                let len = String.length s - tabpos - 1 in
                let s1 = String.sub s 0 tabpos
                and s2 = String.sub s (tabpos + 1) len in
                let nx = drawstr x s1 in
                let sw = nx -. x in
                let x = x +. (max tabw sw) in
                drawstr x s2
              else
                let len = String.length s - 2 in
                if len > 0 && s.[0] = '\xc2' && s.[1] = '\xb7'
                then
                  let s = String.sub s 2 len in
                  let x = if not helpmode then x +. ww else x in
                  GlDraw.color (1.2, 1.2, 1.2);
                  let vinc = drawstring1 (fs+fs/4)
                      (truncate (x -. ww)) (y+nfs) s in
                  GlDraw.color (1., 1., 1.);
                  vinc +. (float fs *. 0.8)
                else
                  drawstr x s
            else
              drawstr x s
          in
          ignore (drawtabularstring s);
          loop (row+1)
        )
      )
    in
    loop m_first;
    GlDraw.color (1.0, 1.0, 1.0) ~alpha:0.5;
    let xadj = 5.0 in
    let rec loop row =
      if (row - m_first) > fstate.maxrows
      then ()
      else (
        if row >= 0 && row < itemcount
        then (
          let (s, level) = source#getitem row in
          let pos0 = nindex s '\000' in
          let y = (row - m_first) * nfs in
          let x = float (level + m_pan) *. ww in
          let (first, last) = minfo.(row) in
          let prefix =
            if pos0 > 0 && first > pos0
            then String.sub s (pos0+1) (first-pos0-1)
            else String.sub s 0 first
          in
          let suffix = String.sub s first (last - first) in
          let w1 = measurestr fstate.fontsize prefix in
          let w2 = measurestr fstate.fontsize suffix in
          let x = x +. if conf.leftscroll then xadj else 5.0 in
          let x = if pos0 > 0 && first > pos0 then x +. float hw else x in
          let x0 = x +. w1
          and y0 = float (y+2) in
          let x1 = x0 +. w2
          and y1 = float (y+fs+3) in
          filledrect x0 y0 x1 y1;
          loop (row+1)
        )
      )
    in
    Gl.disable `texture_2d;
    if Array.length minfo > 0 then loop m_first;
    Gl.disable `blend;
    if conf.leftscroll
    then GlMat.pop ();

  method updownlevel incr =
    let len = source#getitemcount in
    let curlevel =
      if m_active >= 0 && m_active < len
      then snd (source#getitem m_active)
      else -1
    in
    let rec flow i =
      if i = len then i-1 else if i = -1 then 0 else
          let _, l = source#getitem i in
          if l != curlevel then i else flow (i+incr)
    in
    let active = flow m_active in
    let first = calcfirst m_first active in
    G.postRedisplay "outline updownlevel";
    {< m_active = active; m_first = first >}

  method private key1 key mask =
    let set1 active first qsearch =
      coe {< m_active = active; m_first = first; m_qsearch = qsearch >}
    in
    let search active pattern incr =
      let active = if active = -1 then m_first else active in
      let dosearch re =
        let rec loop n =
          if n >= 0 && n < source#getitemcount
          then (
            let s, _ = source#getitem n in
            match Str.search_forward re s 0 with
            | (exception Not_found) -> loop (n + incr)
            | _ -> Some n
          )
          else None
        in
        loop active
      in
      let qpat = Str.quote pattern in
      match Str.regexp_case_fold qpat with
      | s -> dosearch s
      | exception exn ->
         adderrfmt "listview key1" "regexp_case_fold for `%S' failed: %S\n"
                   qpat @@ Printexc.to_string exn;
         None
    in
    let itemcount = source#getitemcount in
    let find start incr =
      let rec find i =
        if i = -1 || i = itemcount
        then -1
        else (
          if source#hasaction i
          then i
          else find (i + incr)
        )
      in
      find start
    in
    let set active first =
      let first = bound first 0 (itemcount - fstate.maxrows) in
      state.text <- E.s;
      coe {< m_active = active; m_first = first; m_qsearch = E.s >}
    in
    let navigate incr =
      let isvisible first n = n >= first && n - first <= fstate.maxrows in
      let active, first =
        let incr1 = if incr > 0 then 1 else -1 in
        if isvisible m_first m_active
        then
          let next =
            let next = m_active + incr in
            let next =
              if next < 0 || next >= itemcount
              then -1
              else find next incr1
            in
            if abs (m_active - next) > fstate.maxrows
            then -1
            else next
          in
          if next = -1
          then
            let first = m_first + incr in
            let first = bound first 0 (itemcount - fstate.maxrows) in
            let next =
              let next = m_active + incr in
              let next = bound next 0 (itemcount - 1) in
              find next ~-incr1
            in
            let active =
              if next = -1
              then m_active
              else (
                if isvisible first next
                then next
                else m_active
              )
            in
            active, first
          else
            let first = min next m_first in
            let first =
              if abs (next - first) > fstate.maxrows
              then first + incr
              else first
            in
            next, first
        else
          let first = m_first + incr in
          let first = bound first 0 (itemcount - 1) in
          let active =
            let next = m_active + incr in
            let next = bound next 0 (itemcount - 1) in
            let next = find next incr1 in
            let active =
              if next = -1 || abs (m_active - first) > fstate.maxrows
              then (
                let active = if m_active = -1 then next else m_active in
                active
              )
              else next
            in
            if isvisible first active
            then active
            else -1
          in
          active, first
      in
      G.postRedisplay "listview navigate";
      set active first;
    in
    match key with
    | (@r|@s) when Wsi.withctrl mask ->
        let incr = if key = @r then -1 else 1 in
        let active, first =
          match search (m_active + incr) m_qsearch incr with
          | None ->
              state.text <- m_qsearch ^ " [not found]";
              m_active, m_first
          | Some active ->
              state.text <- m_qsearch;
              active, firstof m_first active
        in
        G.postRedisplay "listview ctrl-r/s";
        set1 active first m_qsearch;

    | @insert when Wsi.withctrl mask ->
        if m_active >= 0 && m_active < source#getitemcount
        then (
          let s, _ = source#getitem m_active in
          selstring s;
        );
        coe self

    | @backspace ->
        if emptystr m_qsearch
        then coe self
        else (
          let qsearch = withoutlastutf8 m_qsearch in
          if emptystr qsearch
          then (
            state.text <- E.s;
            G.postRedisplay "listview empty qsearch";
            set1 m_active m_first E.s;
          )
          else
            let active, first =
              match search m_active qsearch ~-1 with
              | None ->
                  state.text <- qsearch ^ " [not found]";
                  m_active, m_first
              | Some active ->
                  state.text <- qsearch;
                  active, firstof m_first active
            in
            G.postRedisplay "listview backspace qsearch";
            set1 active first qsearch
        );

    | key when (key != 0 && not (Wsi.isspecialkey key)) ->
        let pattern = m_qsearch ^ toutf8 key in
        let active, first =
          match search m_active pattern 1 with
          | None ->
              state.text <- pattern ^ " [not found]";
              m_active, m_first
          | Some active ->
              state.text <- pattern;
              active, firstof m_first active
        in
        G.postRedisplay "listview qsearch add";
        set1 active first pattern;

    | @escape ->
        state.text <- E.s;
        if emptystr m_qsearch
        then (
          G.postRedisplay "list view escape";
          let mx, my = state.mpos in
          updateunder mx my;
          begin
            match
              source#exit ~uioh:(coe self)
                ~cancel:true ~active:m_active ~first:m_first ~pan:m_pan
            with
            | None -> m_prev_uioh
            | Some uioh -> uioh
          end
        )
        else (
          G.postRedisplay "list view kill qsearch";
          coe {< m_qsearch = E.s >}
        )

    | @enter | @kpenter ->
        state.text <- E.s;
        let self = {< m_qsearch = E.s >} in
        let opt =
          G.postRedisplay "listview enter";
          if m_active >= 0 && m_active < source#getitemcount
          then (
            source#exit ~uioh:(coe self) ~cancel:false
              ~active:m_active ~first:m_first ~pan:m_pan;
          )
          else (
            source#exit ~uioh:(coe self) ~cancel:true
              ~active:m_active ~first:m_first ~pan:m_pan;
          );
        in
        begin match opt with
        | None -> m_prev_uioh
        | Some uioh -> uioh
        end

    | @delete | @kpdelete ->
        coe self

    | @up | @kpup -> navigate ~-1
    | @down | @kpdown -> navigate 1
    | @prior | @kpprior -> navigate ~-(fstate.maxrows)
    | @next | @kpnext -> navigate fstate.maxrows

    | @right | @kpright ->
        state.text <- E.s;
        G.postRedisplay "listview right";
        coe {< m_pan = m_pan - 1 >}

    | @left | @kpleft ->
        state.text <- E.s;
        G.postRedisplay "listview left";
        coe {< m_pan = m_pan + 1 >}

    | @home | @kphome ->
        let active = find 0 1 in
        G.postRedisplay "listview home";
        set active 0;

    | @jend | @kpend ->
        let first = max 0 (itemcount - fstate.maxrows) in
        let active = find (itemcount - 1) ~-1 in
        G.postRedisplay "listview end";
        set active first;

    | key when (key = 0 || Wsi.isspecialkey key) ->
        coe self

    | _ ->
        dolog "listview unknown key %#x" key; coe self

  method key key mask =
    match state.mode with
    | Textentry te -> textentrykeyboard key mask te; coe self
    | Birdseye _
    | View
    | LinkNav _ -> self#key1 key mask

  method button button down x y _ =
    let opt =
      match button with
      | 1 when vscrollhit x ->
          G.postRedisplay "listview scroll";
          if down
          then
            let _, position, sh = self#scrollph in
            if y > truncate position && y < truncate (position +. sh)
            then (
              state.mstate <- Mscrolly;
              Some (coe self)
            )
            else
              let s = float (max 0 (y - conf.scrollh)) /. float state.winh in
              let first = truncate (s *. float source#getitemcount) in
              let first = min source#getitemcount first in
              Some (coe {< m_first = first; m_active = first >})
          else (
            state.mstate <- Mnone;
            Some (coe self);
          );
      | 1 when down ->
          begin match self#elemunder y with
          | Some n ->
              G.postRedisplay "listview click";
              source#exit ~uioh:(coe {< m_active = n >})
                ~cancel:false ~active:n ~first:m_first ~pan:m_pan
          | _ ->
              Some (coe self)
          end
      | n when (n == 4 || n == 5) && not down ->
          let len = source#getitemcount in
          let first =
            if n = 5 && m_first + fstate.maxrows >= len
            then
              m_first
            else
              let first = m_first + (if n == 4 then -1 else 1) in
              bound first 0 (len - 1)
          in
          G.postRedisplay "listview wheel";
          Some (coe {< m_first = first >})
      | n when (n = 6 || n = 7) && not down ->
          let inc = if n = 7 then -1 else 1 in
          G.postRedisplay "listview hwheel";
          Some (coe {< m_pan = m_pan + inc >})
      | _ ->
          Some (coe self)
    in
    match opt with
    | None -> m_prev_uioh
    | Some uioh -> uioh

  method multiclick _ x y = self#button 1 true x y

  method motion _ y =
    match state.mstate with
    | Mscrolly ->
        let s = float (max 0 (y - conf.scrollh)) /. float state.winh in
        let first = truncate (s *. float source#getitemcount) in
        let first = min source#getitemcount first in
        G.postRedisplay "listview motion";
        coe {< m_first = first; m_active = first >}
    | Msel _
    | Mpan _
    | Mscrollx
    | Mzoom _
    | Mzoomrect _
    | Mnone -> coe self

  method pmotion x y =
    if x < state.winw - conf.scrollbw
    then
      let n =
        match self#elemunder y with
        | None -> Wsi.setcursor Wsi.CURSOR_INHERIT; m_active
        | Some n -> Wsi.setcursor Wsi.CURSOR_INFO; n
      in
      let o =
        if n != m_active
        then (G.postRedisplay "listview pmotion"; {< m_active = n >})
        else self
      in
      coe o
    else (
      Wsi.setcursor Wsi.CURSOR_INHERIT;
      coe self
    )

  method infochanged _ = ()

  method scrollpw = (0, 0.0, 0.0)
  method scrollph =
    let nfs = fstate.fontsize + 1 in
    let y = m_first * nfs in
    let itemcount = source#getitemcount in
    let maxi = max 0 (itemcount - fstate.maxrows) in
    let maxy = maxi * nfs in
    let p, h = scrollph y maxy in
    conf.scrollbw, p, h

  method modehash = modehash
  method eformsgs = false
  method alwaysscrolly = true
end;;

class outlinelistview ~zebra ~source =
  let settext autonarrow s =
    if autonarrow
    then
      let ss = source#statestr in
      state.text <-
        if emptystr ss
        then "[" ^ s ^ "]"
        else "{" ^ ss ^ "} [" ^ s ^ "]"
    else state.text <- s
  in
object (self)
  inherit listview
    ~zebra
    ~helpmode:false
    ~source:(source :> lvsource)
    ~trusted:false
    ~modehash:(findkeyhash conf "outline")
    as super

  val m_autonarrow = false

  method! key key mask =
    let maxrows =
      if emptystr state.text
      then fstate.maxrows
      else fstate.maxrows - 2
    in
    let calcfirst first active =
      if active > first
      then
        let rows = active - first in
        if rows > maxrows then active - maxrows else first
      else active
    in
    let navigate incr =
      let active = m_active + incr in
      let active = bound active 0 (source#getitemcount - 1) in
      let first = calcfirst m_first active in
      G.postRedisplay "outline navigate";
      coe {< m_active = active; m_first = first >}
    in
    let navscroll first =
      let active =
        let dist = m_active - first in
        if dist < 0
        then first
        else (
          if dist < maxrows
          then m_active
          else first + maxrows
        )
      in
      G.postRedisplay "outline navscroll";
      coe {< m_first = first; m_active = active >}
    in
    let ctrl = Wsi.withctrl mask in
    match key with
    | @a when ctrl ->
        let text =
          if m_autonarrow
          then (source#denarrow; E.s)
          else (
            let pattern = source#renarrow in
            if nonemptystr m_qsearch
            then (source#narrow m_qsearch; m_qsearch)
            else pattern
          )
        in
        settext (not m_autonarrow) text;
        G.postRedisplay "toggle auto narrowing";
        coe {< m_first = 0; m_active = 0; m_autonarrow = not m_autonarrow >}

    | @slash when emptystr m_qsearch && not m_autonarrow ->
        settext true E.s;
        G.postRedisplay "toggle auto narrowing";
        coe {< m_first = 0; m_active = 0; m_autonarrow = true >}

    | @n when ctrl ->
        source#narrow m_qsearch;
        if not m_autonarrow
        then source#add_narrow_pattern m_qsearch;
        G.postRedisplay "outline ctrl-n";
        coe {< m_first = 0; m_active = 0 >}

    | @S when ctrl ->
        let active = source#calcactive (getanchor ()) in
        let first = firstof m_first active in
        G.postRedisplay "outline ctrl-s";
        coe {< m_first = first; m_active = active >}

    | @u when ctrl ->
        G.postRedisplay "outline ctrl-u";
        if m_autonarrow && nonemptystr m_qsearch
        then (
          ignore (source#renarrow);
          settext m_autonarrow E.s;
          coe {< m_first = 0; m_active = 0; m_qsearch = E.s >}
        )
        else (
          source#del_narrow_pattern;
          let pattern = source#renarrow in
          let text =
            if emptystr pattern then E.s else "Narrowed to " ^ pattern
          in
          settext m_autonarrow text;
          coe {< m_first = 0; m_active = 0; m_qsearch = E.s >}
        )

    | @l when ctrl ->
        let first = max 0 (m_active - (fstate.maxrows / 2)) in
        G.postRedisplay "outline ctrl-l";
        coe {< m_first = first >}

    | @tab when m_autonarrow ->
        if nonemptystr m_qsearch
        then (
          G.postRedisplay "outline list view tab";
          source#add_narrow_pattern m_qsearch;
          settext true E.s;
          coe {< m_qsearch = E.s >}
        )
        else coe self

    | @escape when m_autonarrow ->
        if nonemptystr m_qsearch
        then source#add_narrow_pattern m_qsearch;
        super#key key mask

    | @enter | @kpenter when m_autonarrow ->
        if nonemptystr m_qsearch
        then source#add_narrow_pattern m_qsearch;
        super#key key mask

    | key when m_autonarrow && (not (Wsi.isspecialkey key)) ->
        let pattern = m_qsearch ^ toutf8 key in
        G.postRedisplay "outlinelistview autonarrow add";
        source#narrow pattern;
        settext true pattern;
        coe {< m_first = 0; m_active = 0; m_qsearch = pattern >}

    | key when m_autonarrow && key = @backspace ->
        if emptystr m_qsearch
        then coe self
        else
          let pattern = withoutlastutf8 m_qsearch in
          G.postRedisplay "outlinelistview autonarrow backspace";
          ignore (source#renarrow);
          source#narrow pattern;
          settext true pattern;
          coe {< m_first = 0; m_active = 0; m_qsearch = pattern >}

    | @up | @kpup when ctrl ->
        navscroll (max 0 (m_first - 1))

    | @down | @kpdown when ctrl ->
        navscroll (min (source#getitemcount - 1) (m_first + 1))

    | @up | @kpup -> navigate ~-1
    | @down | @kpdown -> navigate 1
    | @prior | @kpprior -> navigate ~-(fstate.maxrows)
    | @next | @kpnext -> navigate fstate.maxrows

    | @right | @kpright ->
        let o =
          if ctrl
          then (
            G.postRedisplay "outline ctrl right";
            {< m_pan = m_pan + 1 >}
          )
          else self#updownlevel 1
        in
        coe o

    | @left | @kpleft ->
        let o =
          if ctrl
          then (
            G.postRedisplay "outline ctrl left";
            {< m_pan = m_pan - 1 >}
          )
          else self#updownlevel ~-1
        in
        coe o

    | @home | @kphome ->
        G.postRedisplay "outline home";
        coe {< m_first = 0; m_active = 0 >}

    | @jend | @kpend ->
        let active = source#getitemcount - 1 in
        let first = max 0 (active - fstate.maxrows) in
        G.postRedisplay "outline end";
        coe {< m_active = active; m_first = first >}

    | _ -> super#key key mask
end;;

let genhistoutlines () =
  Config.gethist ()
  |> List.sort (fun (_, c1, _, _, _, _) (_, c2, _, _, _, _) ->
      compare c2.lastvisit c1.lastvisit)
  |> List.map
      (fun ((path, c, _, _, _, origin) as hist) ->
        let path = if nonemptystr origin then origin else path in
        let base = mbtoutf8 @@ Filename.basename path in
        (base ^ "\000" ^ c.title, 1, Ohistory hist)
      )
  |> Array.of_list
;;

let gotohist (path, c, bookmarks, x, anchor, origin) =
  Config.save leavebirdseye;
  state.anchor <- anchor;
  state.bookmarks <- bookmarks;
  state.origin <- origin;
  state.x <- x;
  setconf conf c;
  let x0, y0, x1, y1 = conf.trimfuzz in
  wcmd "trimset %d %d %d %d %d" (btod conf.trimmargins) x0 y0 x1 y1;
  reshape ~firsttime:true state.winw state.winh;
  opendoc path origin;
  setzoom c.zoom;
;;

let makecheckers () =
  (* Based on lablGL-1.04/LablGlut/examples/lablGL/checker.ml which had
     following to say:
     converted by Issac Trotts.  July 25, 2002 *)
  let image = GlPix.create `ubyte ~format:`luminance ~width:2 ~height:2 in
  Raw.sets_string (GlPix.to_raw image) ~pos:0 "\255\200\200\255";
  let id = GlTex.gen_texture () in
  GlTex.bind_texture ~target:`texture_2d id;
  GlPix.store (`unpack_alignment 1);
  GlTex.image2d image;
  List.iter (GlTex.parameter ~target:`texture_2d)
    [ `mag_filter `nearest; `min_filter `nearest ];
  id;
;;

let setcheckers enabled =
  match state.checkerstexid with
  | None ->
      if enabled then state.checkerstexid <- Some (makecheckers ())

  | Some checkerstexid ->
      if not enabled
      then (
        GlTex.delete_texture checkerstexid;
        state.checkerstexid <- None;
      );
;;

let describe_location () =
  let fn = page_of_y state.y in
  let ln = page_of_y (state.y +. float (state.winh - 1)) in
  let maxy = state.maxy -. float (if conf.maxhfit then state.winh else 0) in
  let percent =
    if maxy <= 0.0
    then 100.
    else (100. *. (state.y /. maxy))
  in
  if fn = ln
  then
    Printf.sprintf "page %d of %d [%.2f%%]"
      (fn+1) state.pagecount percent
  else
    Printf.sprintf
      "pages %d-%d of %d [%.2f%%]"
      (fn+1) (ln+1) state.pagecount percent
;;

let setpresentationmode v =
  let n = page_of_y state.y in
  state.anchor <- (n, 0.0, 1.0);
  conf.presentation <- v;
  if conf.fitmodel = FitPage
  then reqlayout conf.angle conf.fitmodel;
  represent ();
;;

let setbgcol (r, g, b) =
  let col =
    let r = r *. 255.0 |> truncate
    and g = g *. 255.0 |> truncate
    and b = b *. 255.0 |> truncate in
    r lsl 16 |> (lor) (g lsl 8) |> (lor) b
  in
  Wsi.setwinbgcol col;
;;

let enterinfomode =
  let btos b = if b then "@Uradical" else E.s in
  let showextended = ref false in
  let leave mode _ =  state.mode <- mode in
  let src =
    (object
      val mutable m_l = []
      val mutable m_a = E.a
      val mutable m_prev_uioh = nouioh
      val mutable m_prev_mode = View

      inherit lvsourcebase

      method reset prev_mode prev_uioh =
        m_a <- Array.of_list (List.rev m_l);
        m_l <- [];
        m_prev_mode <- prev_mode;
        m_prev_uioh <- prev_uioh;

      method int name get set =
        m_l <-
          (name, `int get, 1, Action (
            fun u ->
              let ondone s =
                try set (int_of_string s)
                with exn ->
                  state.text <- Printf.sprintf "bad integer `%s': %s"
                    s @@ exntos exn
              in
              state.text <- E.s;
              let te = name ^ ": ", E.s, None, intentry, ondone, true in
              state.mode <- Textentry (te, leave m_prev_mode);
              u
          )) :: m_l

      method int_with_suffix name get set =
        m_l <-
          (name, `intws get, 1, Action (
            fun u ->
              let ondone s =
                try set (int_of_string_with_suffix s)
                with exn ->
                  state.text <- Printf.sprintf "bad integer `%s': %s"
                    s @@ exntos exn
              in
              state.text <- E.s;
              let te =
                name ^ ": ", E.s, None, intentry_with_suffix, ondone, true
              in
              state.mode <- Textentry (te, leave m_prev_mode);
              u
          )) :: m_l

      method bool ?(offset=1) ?(btos=btos) name get set =
        m_l <-
          (name, `bool (btos, get), offset, Action (
            fun u ->
              let v = get () in
              set (not v);
              u
          )) :: m_l

      method color name get set =
        m_l <-
          (name, `color get, 1, Action (
            fun u ->
              let invalid = (nan, nan, nan) in
              let ondone s =
                let c =
                  try color_of_string s
                  with exn ->
                    state.text <- Printf.sprintf "bad color `%s': %s"
                      s @@ exntos exn;
                    invalid
                in
                if c <> invalid
                then set c;
              in
              let te = name ^ ": ", E.s, None, textentry, ondone, true in
              state.text <- color_to_string (get ());
              state.mode <- Textentry (te, leave m_prev_mode);
              u
          )) :: m_l

      method string name get set =
        m_l <-
          (name, `string get, 1, Action (
            fun u ->
              let ondone s = set s in
              let te = name ^ ": ", E.s, None, textentry, ondone, true in
              state.mode <- Textentry (te, leave m_prev_mode);
              u
          )) :: m_l

      method colorspace name get set =
        m_l <-
          (name, `string get, 1, Action (
            fun _ ->
              let source =
                (object
                  inherit lvsourcebase

                  initializer
                    m_active <- CSTE.to_int conf.colorspace;
                    m_first <- 0;

                  method getitemcount =
                    Array.length CSTE.names
                  method getitem n =
                    (CSTE.names.(n), 0)
                  method exit ~uioh ~cancel ~active ~first ~pan =
                    ignore (uioh, first, pan);
                    if not cancel then set active;
                    None
                  method hasaction _ = true
                end)
              in
              state.text <- E.s;
              let modehash = findkeyhash conf "info" in
              coe (new listview ~zebra:false ~helpmode:false
                     ~source ~trusted:true ~modehash)
          )) :: m_l

      method paxmark name get set =
        m_l <-
          (name, `string get, 1, Action (
            fun _ ->
              let source =
                (object
                  inherit lvsourcebase

                  initializer
                    m_active <- MTE.to_int conf.paxmark;
                    m_first <- 0;

                  method getitemcount = Array.length MTE.names
                  method getitem n = (MTE.names.(n), 0)
                  method exit ~uioh ~cancel ~active ~first ~pan =
                    ignore (uioh, first, pan);
                    if not cancel then set active;
                    None
                  method hasaction _ = true
                end)
              in
              state.text <- E.s;
              let modehash = findkeyhash conf "info" in
              coe (new listview ~zebra:false ~helpmode:false
                     ~source ~trusted:true ~modehash)
          )) :: m_l

      method fitmodel name get set =
        m_l <-
          (name, `string get, 1, Action (
            fun _ ->
              let source =
                (object
                  inherit lvsourcebase

                  initializer
                    m_active <- FMTE.to_int conf.fitmodel;
                    m_first <- 0;

                  method getitemcount = Array.length FMTE.names
                  method getitem n = (FMTE.names.(n), 0)
                  method exit ~uioh ~cancel ~active ~first ~pan =
                    ignore (uioh, first, pan);
                    if not cancel then set active;
                    None
                  method hasaction _ = true
                end)
              in
              state.text <- E.s;
              let modehash = findkeyhash conf "info" in
              coe (new listview ~zebra:false ~helpmode:false
                     ~source ~trusted:true ~modehash)
          )) :: m_l

      method caption s offset =
        m_l <- (s, `empty, offset, Noaction) :: m_l

      method caption2 s f offset =
        m_l <- (s, `string f, offset, Noaction) :: m_l

      method getitemcount = Array.length m_a

      method getitem n =
        let tostr = function
          | `int f -> string_of_int (f ())
          | `intws f -> string_with_suffix_of_int (f ())
          | `string f -> f ()
          | `color f -> color_to_string (f ())
          | `bool (btos, f) -> btos (f ())
          | `empty -> E.s
        in
        let name, t, offset, _ = m_a.(n) in
        ((let s = tostr t in
          if nonemptystr s
          then Printf.sprintf "%s\t%s" name s
          else name),
        offset)

      method exit ~uioh ~cancel ~active ~first ~pan =
        let uiohopt =
          if not cancel
          then (
            let uioh =
              match m_a.(active) with
              | _, _, _, Action f -> f uioh
              | _, _, _, Noaction -> uioh
            in
            Some uioh
          )
          else None
        in
        m_active <- active;
        m_first <- first;
        m_pan <- pan;
        uiohopt

      method hasaction n =
        match m_a.(n) with
        | _, _, _, Action _ -> true
        | _, _, _, Noaction -> false

      initializer m_active <- 1
    end)
  in
  let rec fillsrc prevmode prevuioh =
    let sep () = src#caption E.s 0 in
    let colorp name get set =
      src#string name
        (fun () -> color_to_string (get ()))
        (fun v ->
          try
            let c = color_of_string v in
            set c
          with exn ->
            state.text <- Printf.sprintf "bad color `%s': %s" v @@ exntos exn
        )
    in
    let oldmode = state.mode in
    let birdseye = isbirdseye state.mode in

    src#caption (if birdseye then "Setup (Bird's eye)" else "Setup") 0;

    src#bool "presentation mode"
      (fun () -> conf.presentation)
      (fun v -> setpresentationmode v);

    src#bool "ignore case in searches"
      (fun () -> conf.icase)
      (fun v -> conf.icase <- v);

    src#bool "preload"
      (fun () -> conf.preload)
      (fun v -> conf.preload <- v);

    src#bool "highlight links"
      (fun () -> conf.hlinks)
      (fun v -> conf.hlinks <- v);

    src#bool "under info"
      (fun () -> conf.underinfo)
      (fun v -> conf.underinfo <- v);

    src#bool "persistent bookmarks"
      (fun () -> conf.savebmarks)
      (fun v -> conf.savebmarks <- v);

    src#fitmodel "fit model"
      (fun () -> FMTE.to_string conf.fitmodel)
      (fun v -> reqlayout conf.angle (FMTE.of_int v));

    src#bool "trim margins"
      (fun () -> conf.trimmargins)
      (fun v -> settrim v conf.trimfuzz; fillsrc prevmode prevuioh);

    src#bool "persistent location"
      (fun () -> conf.jumpback)
      (fun v -> conf.jumpback <- v);

    sep ();
    src#int "inter-page space"
      (fun () -> conf.interpagespace)
      (fun n ->
        conf.interpagespace <- n;
        docolumns conf.columns;
        let pageno, py =
          match state.layout with
          | [] -> 0, 0
          | l :: _ ->
              l.pageno, l.pagey
        in
        state.maxy <- calcheight ();
        let y = getpagey pageno in
        gotoxy state.x (y +. float py)
      );

    src#int "page bias"
      (fun () -> conf.pagebias)
      (fun v -> conf.pagebias <- v);

    src#int "scroll step"
      (fun () -> conf.scrollstep)
      (fun n -> conf.scrollstep <- n);

    src#int "horizontal scroll step"
      (fun () -> conf.hscrollstep)
      (fun v -> conf.hscrollstep <- v);

    src#int "auto scroll step"
      (fun () ->
        match state.autoscroll with
        | Some step -> step
        | _ -> conf.autoscrollstep)
      (fun n ->
        let n = boundastep state.winh n in
        if state.autoscroll <> None
        then state.autoscroll <- Some n;
        conf.autoscrollstep <- n);

    src#int "zoom"
      (fun () -> truncate (conf.zoom *. 100.))
      (fun v -> pivotzoom ((float v) /. 100.));

    src#int "rotation"
      (fun () -> conf.angle)
      (fun v -> reqlayout v conf.fitmodel);

    src#int "scroll bar width"
      (fun () -> conf.scrollbw)
      (fun v ->
        conf.scrollbw <- v;
        reshape state.winw state.winh;
      );

    src#int "scroll handle height"
      (fun () -> conf.scrollh)
      (fun v -> conf.scrollh <- v;);

    src#int "thumbnail width"
      (fun () -> conf.thumbw)
      (fun v ->
        conf.thumbw <- min 4096 v;
        match oldmode with
        | Birdseye beye ->
            leavebirdseye beye false;
            enterbirdseye ()
        | Textentry _
        | View
        | LinkNav _ -> ()
      );

    let mode = state.mode in
    src#string "columns"
      (fun () ->
        match conf.columns with
        | Csingle _ -> "1"
        | Cmulti (multi, _) -> multicolumns_to_string multi
        | Csplit (count, _) -> "-" ^ string_of_int count
      )
      (fun v ->
        let n, a, b = multicolumns_of_string v in
        setcolumns mode n a b);

    sep ();
    src#caption "Pixmap cache" 0;
    src#int_with_suffix "size (advisory)"
      (fun () -> conf.memlimit)
      (fun v -> conf.memlimit <- v);

    src#caption2 "used"
      (fun () -> Printf.sprintf "%s bytes, %d tiles"
        (string_with_suffix_of_int state.memused)
        (Hashtbl.length state.tilemap)) 1;

    sep ();
    src#caption "Layout" 0;
    src#caption2 "Dimension"
      (fun () ->
        Printf.sprintf "%dx%d (virtual %dx%f)"
          state.winw state.winh
          state.w state.maxy)
      1;
    if conf.debug
    then
      src#caption2 "Position" (fun () ->
        Printf.sprintf "%fx%f" state.x state.y
      ) 1
    else
      src#caption2 "Position" (fun () -> describe_location ()) 1
    ;

    sep ();
    src#bool ~offset:0 ~btos:(fun v -> if v then "(on)" else "(off)")
      "Save these parameters as global defaults at exit"
      (fun () -> conf.bedefault)
      (fun v -> conf.bedefault <- v)
    ;

    sep ();
    let btos b = if b then "@Ulguillemet" else "@Urguillemet" in
    src#bool ~offset:0 ~btos "Extended parameters"
      (fun () -> !showextended)
      (fun v -> showextended := v; fillsrc prevmode prevuioh);
    if !showextended
    then (
      src#bool "checkers"
        (fun () -> conf.checkers)
        (fun v -> conf.checkers <- v; setcheckers v);
      src#bool "update cursor"
        (fun () -> conf.updatecurs)
        (fun v -> conf.updatecurs <- v);
      src#bool "scroll-bar on the left"
        (fun () -> conf.leftscroll)
        (fun v -> conf.leftscroll <- v);
      src#bool "verbose"
        (fun () -> conf.verbose)
        (fun v -> conf.verbose <- v);
      src#bool "invert colors"
        (fun () -> conf.invert)
        (fun v -> conf.invert <- v);
      src#bool "max fit"
        (fun () -> conf.maxhfit)
        (fun v -> conf.maxhfit <- v);
      src#bool "pax mode"
        (fun () -> conf.pax != None)
        (fun v ->
          if v
          then conf.pax <- Some (ref (now (), 0, 0))
          else conf.pax <- None);
      src#string "uri launcher"
        (fun () -> conf.urilauncher)
        (fun v -> conf.urilauncher <- v);
      src#string "path launcher"
        (fun () -> conf.pathlauncher)
        (fun v -> conf.pathlauncher <- v);
      src#string "tile size"
        (fun () -> Printf.sprintf "%dx%d" conf.tilew conf.tileh)
        (fun v ->
          try
            let w, h = Scanf.sscanf v "%dx%d" (fun w h -> w, h) in
            conf.tilew <- max 64 w;
            conf.tileh <- max 64 h;
            flushtiles ();
          with exn ->
            state.text <- Printf.sprintf "bad tile size `%s': %s"
              v @@ exntos exn
        );
      src#int "texture count"
        (fun () -> conf.texcount)
        (fun v ->
          if realloctexts v
          then conf.texcount <- v
          else impmsg "failed to set texture count please retry later"
        );
      src#int "slice height"
        (fun () -> conf.sliceheight)
        (fun v ->
          conf.sliceheight <- v;
          wcmd "sliceh %d" conf.sliceheight;
        );
      src#int "anti-aliasing level"
        (fun () -> conf.aalevel)
        (fun v ->
          conf.aalevel <- bound v 0 8;
          state.anchor <- getanchor ();
          opendoc state.path state.password;
        );
      src#string "page scroll scaling factor"
        (fun () -> string_of_float conf.pgscale)
        (fun v ->
          try
            let s = float_of_string v in
            conf.pgscale <- s
          with exn ->
            state.text <- Printf.sprintf
              "bad page scroll scaling factor `%s': %s" v @@ exntos exn
        )
      ;
      src#int "ui font size"
        (fun () -> fstate.fontsize)
        (fun v -> setfontsize (bound v 5 100));
      src#int "hint font size"
        (fun () -> conf.hfsize)
        (fun v -> conf.hfsize <- bound v 5 100);
      colorp "background color"
        (fun () -> conf.bgcolor)
        (fun v -> conf.bgcolor <- v; setbgcol v);
      src#bool "crop hack"
        (fun () -> conf.crophack)
        (fun v -> conf.crophack <- v);
      src#string "trim fuzz"
        (fun () -> irect_to_string conf.trimfuzz)
        (fun v ->
          try
            conf.trimfuzz <- irect_of_string v;
            if conf.trimmargins
            then settrim true conf.trimfuzz;
          with exn ->
            state.text <- Printf.sprintf "bad irect `%s': %s" v @@ exntos exn
        );
      src#string "throttle"
        (fun () ->
          match conf.maxwait with
          | None -> "show place holder if page is not ready"
          | Some time ->
              if time = infinity
              then "wait for page to fully render"
              else
                "wait " ^ string_of_float time
                ^ " seconds before showing placeholder"
        )
        (fun v ->
          try
            let f = float_of_string v in
            if f <= 0.0
            then conf.maxwait <- None
            else conf.maxwait <- Some f
          with exn ->
            state.text <- Printf.sprintf "bad time `%s': %s" v @@ exntos exn
        );
      src#string "ghyll scroll"
        (fun () ->
          match conf.ghyllscroll with
          | None -> E.s
          | Some nab -> ghyllscroll_to_string nab
        )
        (fun v ->
          try conf.ghyllscroll <- ghyllscroll_of_string v
          with
          | Failure msg ->
             state.text <- Printf.sprintf "bad ghyll `%s': %s" v msg
          | exn ->
             state.text <- Printf.sprintf "bad ghyll `%s': %s" v @@ exntos exn
        );
      src#string "selection command"
        (fun () -> conf.selcmd)
        (fun v -> conf.selcmd <- v);
      src#string "synctex command"
        (fun () -> conf.stcmd)
        (fun v -> conf.stcmd <- v);
      src#string "pax command"
        (fun () -> conf.paxcmd)
        (fun v -> conf.paxcmd <- v);
      src#string "ask password command"
        (fun () -> conf.passcmd)
        (fun v -> conf.passcmd <- v);
      src#string "save path command"
        (fun () -> conf.savecmd)
        (fun v -> conf.savecmd <- v);
      src#colorspace "color space"
        (fun () -> CSTE.to_string conf.colorspace)
        (fun v ->
          conf.colorspace <- CSTE.of_int v;
          wcmd "cs %d" v;
          load state.layout;
        );
      src#paxmark "pax mark method"
        (fun () -> MTE.to_string conf.paxmark)
        (fun v -> conf.paxmark <- MTE.of_int v);
      if bousable () && !opengl_has_pbo
      then
        src#bool "use PBO"
          (fun () -> conf.usepbo)
          (fun v -> conf.usepbo <- v);
      src#bool "mouse wheel scrolls pages"
        (fun () -> conf.wheelbypage)
        (fun v -> conf.wheelbypage <- v);
      src#bool "open remote links in a new instance"
        (fun () -> conf.riani)
        (fun v -> conf.riani <- v);
      src#bool "edit annotations inline"
        (fun () -> conf.annotinline)
        (fun v -> conf.annotinline <- v);
      src#bool "coarse positioning in presentation mode"
        (fun () -> conf.coarseprespos)
        (fun v -> conf.coarseprespos <- v);
    );

    sep ();
    src#caption "Document" 0;
    List.iter (fun (_, s) -> src#caption s 1) state.docinfo;
    src#caption2 "Pages"
      (fun () ->  string_of_int state.pagecount) 1;
    src#caption2 "Dimensions"
      (fun () -> string_of_int (List.length state.pdims)) 1;
    if conf.trimmargins
    then (
      sep ();
      src#caption "Trimmed margins" 0;
      src#caption2 "Dimensions"
        (fun () -> string_of_int (List.length state.pdims)) 1;
    );

    sep ();
    src#caption "OpenGL" 0;
    src#caption (Printf.sprintf "Vendor\t%s" (GlMisc.get_string `vendor)) 1;
    src#caption (Printf.sprintf "Renderer\t%s" (GlMisc.get_string `renderer)) 1;

    sep ();
    src#caption "Location" 0;
    if nonemptystr state.origin
    then src#caption ("Orign\t" ^ mbtoutf8 state.origin) 1;
    src#caption ("Path\t" ^ mbtoutf8 state.path) 1;

    src#reset prevmode prevuioh;
  in
  fun () ->
    state.text <- E.s;
    resetmstate ();
    let prevmode = state.mode
    and prevuioh = state.uioh in
    fillsrc prevmode prevuioh;
    let source = (src :> lvsource) in
    let modehash = findkeyhash conf "info" in
    state.uioh <- coe (object (self)
      inherit listview ~zebra:false ~helpmode:false
          ~source ~trusted:true ~modehash as super
      val mutable m_prevmemused = 0
      method! infochanged = function
        | Memused ->
            if m_prevmemused != state.memused
            then (
              m_prevmemused <- state.memused;
              G.postRedisplay "memusedchanged";
            )
        | Pdim -> G.postRedisplay "pdimchanged"
        | Docinfo -> fillsrc prevmode prevuioh

      method! key key mask =
        if not (Wsi.withctrl mask)
        then
          match key with
          | @left | @kpleft -> coe (self#updownlevel ~-1)
          | @right | @kpright -> coe (self#updownlevel 1)
          | _ -> super#key key mask
        else super#key key mask
    end);
    G.postRedisplay "info";
;;

let enterhelpmode =
  let source =
    (object
      inherit lvsourcebase
      method getitemcount = Array.length state.help
      method getitem n =
        let s, l, _ = state.help.(n) in
        (s, l)

      method exit ~uioh ~cancel ~active ~first ~pan =
        let optuioh =
          if not cancel
          then (
            match state.help.(active) with
            | _, _, Action f -> Some (f uioh)
            | _, _, Noaction -> Some uioh
          )
          else None
        in
        m_active <- active;
        m_first <- first;
        m_pan <- pan;
        optuioh

      method hasaction n =
        match state.help.(n) with
        | _, _, Action _ -> true
        | _, _, Noaction -> false

      initializer
        m_active <- -1
    end)
  in fun () ->
    let modehash = findkeyhash conf "help" in
    resetmstate ();
    state.uioh <- coe (new listview
                         ~zebra:false ~helpmode:true
                         ~source ~trusted:true ~modehash);
    G.postRedisplay "help";
;;

let entermsgsmode =
  let msgsource =
    (object
      inherit lvsourcebase
      val mutable m_items = E.a

      method getitemcount = 1 + Array.length m_items

      method getitem n =
        if n = 0
        then "[Clear]", 0
        else m_items.(n-1), 0

      method exit ~uioh ~cancel ~active ~first ~pan =
        ignore uioh;
        if not cancel
        then (
          if active = 0
          then Buffer.clear state.errmsgs;
        );
        m_active <- active;
        m_first <- first;
        m_pan <- pan;
        None

      method hasaction n =
        n = 0

      method reset =
        state.newerrmsgs <- false;
        let l = Str.split newlinere (Buffer.contents state.errmsgs) in
        m_items <- Array.of_list l

      initializer
        m_active <- 0
    end)
  in fun () ->
    state.text <- E.s;
    resetmstate ();
    msgsource#reset;
    let source = (msgsource :> lvsource) in
    let modehash = findkeyhash conf "listview" in
    state.uioh <- coe (object
      inherit listview ~zebra:false ~helpmode:false
          ~source ~trusted:false ~modehash as super
      method! display =
        if state.newerrmsgs
        then msgsource#reset;
        super#display
    end);
    G.postRedisplay "msgs";
;;

let getusertext s =
  let editor = getenvwithdef "EDITOR" E.s in
  if emptystr editor
  then E.s
  else
    let tmppath = Filename.temp_file "llpp" "note" in
    if nonemptystr s
    then (
      let oc = open_out tmppath in
      output_string oc s;
      close_out oc;
    );
    let execstr = editor ^ " " ^ tmppath in
    let s =
      match spawn execstr [] with
      | (exception exn) ->
         impmsg "spawn(%S) failed: %s" execstr @@ exntos exn;
         E.s
      | pid ->
         match Unix.waitpid [] pid with
         | (exception exn) ->
            impmsg "waitpid(%d) failed: %s" pid @@ exntos exn;
            E.s
         | (_pid, status) ->
            match status with
            | Unix.WEXITED 0 -> filecontents tmppath
            | Unix.WEXITED n ->
               impmsg "editor process(%s) exited abnormally: %d" execstr n;
               E.s
            | Unix.WSIGNALED n ->
               impmsg "editor process(%s) was killed by signal %d" execstr n;
               E.s
            | Unix.WSTOPPED n ->
               impmsg "editor(%s) process was stopped by signal %d" execstr n;
               E.s
    in
    match Unix.unlink tmppath with
    | (exception exn) ->
       impmsg "failed to ulink %S: %s" tmppath @@ exntos exn;
       s
    | () -> s
;;

let enterannotmode opaque slinkindex =
  let msgsource =
    (object
        inherit lvsourcebase
        val mutable m_text = E.s
        val mutable m_items = E.a

        method getitemcount = Array.length m_items

        method getitem n =
          let label, _func = m_items.(n) in
          label, 0

        method exit ~uioh ~cancel ~active ~first ~pan =
          ignore (uioh, first, pan);
          if not cancel
          then (
            let _label, func = m_items.(active) in
            func ()
          );
          None

        method hasaction n = nonemptystr @@ fst m_items.(n)

        method reset s =
          let rec split accu b i =
            let p = b+i in
            if p = String.length s
            then (String.sub s b (p-b), unit) :: accu
            else
              if (i > 70 && s.[p] = ' ') || s.[p] = '\r' || s.[p] = '\n'
              then
                let ss = if i = 0 then E.s else String.sub s b i in
                split ((ss, unit)::accu) (p+1) 0
              else
                split accu b (i+1)
          in
          let cleanup () =
            wcmd "freepage %s" (~> opaque);
            let keys =
              Hashtbl.fold (fun key opaque' accu ->
                            if opaque' = opaque'
                            then key :: accu else accu) state.pagemap []
            in
            List.iter (Hashtbl.remove state.pagemap) keys;
            flushtiles ();
            gotoxy state.x state.y
          in
          let dele () =
            delannot opaque slinkindex;
            cleanup ();
          in
          let edit inline () =
            let update s =
              if emptystr s
              then dele ()
              else (
                modannot opaque slinkindex s;
                cleanup ();
              )
            in
            if inline
            then
              let mode = state.mode in
              state.mode <-
                Textentry (
                    ("annotation: ", m_text, None, textentry, update, true),
                    fun _ -> state.mode <- mode);
              state.text <- E.s;
              enttext ();
            else
              let s = getusertext m_text in
              update s
          in
          m_text <- s;
          m_items <-
            (   "[Copy]", fun () -> selstring m_text)
            :: ("[Delete]", dele)
            :: ("[Edit]", edit conf.annotinline)
            :: (E.s, unit)
            :: split [] 0 0 |> List.rev |> Array.of_list

        initializer
          m_active <- 0
      end)
  in
  state.text <- E.s;
  let s = getannotcontents opaque slinkindex in
  resetmstate ();
  msgsource#reset s;
  let source = (msgsource :> lvsource) in
  let modehash = findkeyhash conf "listview" in
  state.uioh <- coe (object
                        inherit listview ~zebra:false ~helpmode:false
                                         ~source ~trusted:false ~modehash
                      end);
  G.postRedisplay "enterannotmode";
;;

let gotounder under =
  let getpath filename =
    let path =
      if nonemptystr filename
      then
        if Filename.is_relative filename
        then
          let dir = Filename.dirname state.path in
          let dir =
            if Filename.is_implicit dir
            then Filename.concat (Sys.getcwd ()) dir
            else dir
          in
          Filename.concat dir filename
        else filename
      else E.s
    in
    if Sys.file_exists path
    then path
    else E.s
  in
  match under with
  | Ulinkgoto (pageno, top) ->
      if pageno >= 0
      then (
        addnav ();
        let top =
          if conf.presentation && conf.coarseprespos
          then 0
          else top
        in
        gotopage1 pageno top;
      )

  | Ulinkuri s -> gotouri s

  | Uremote (filename, pageno) ->
      let path = getpath filename in
      if nonemptystr path
      then (
        if conf.riani
        then
          let command = Printf.sprintf "%s -page %d %S" !selfexec pageno path in
          match spawn command [] with
          | _pid -> ()
          | (exception exn) ->
             dolog "failed to execute `%s': %s" command @@ exntos exn
        else
          let anchor = getanchor () in
          let ranchor = state.path, state.password, anchor, state.origin in
          state.origin <- E.s;
          state.anchor <- (pageno, 0.0, 0.0);
          state.ranchors <- ranchor :: state.ranchors;
          opendoc path E.s;
      )
      else impmsg "cannot find %s" filename

  | Uremotedest (filename, destname) ->
      let path = getpath filename in
      if nonemptystr path
      then (
        if conf.riani
        then
          let command = !selfexec ^ " " ^ path ^ " -dest " ^ destname in
          match spawn command [] with
          | (exception exn) ->
             dolog "failed to execute `%s': %s" command @@ exntos exn
          | _pid -> ()
        else
          let anchor = getanchor () in
          let ranchor = state.path, state.password, anchor, state.origin in
          state.origin <- E.s;
          state.nameddest <- destname;
          state.ranchors <- ranchor :: state.ranchors;
          opendoc path E.s;
      )
      else impmsg "cannot find %s" filename

  | Uunexpected _ | Ulaunch _ | Unamed _ | Utext _ | Unone -> ()
  | Uannotation (opaque, slinkindex) -> enterannotmode opaque slinkindex
;;

let gotooutline (_, _, kind) =
  match kind with
  | Onone -> ()
  | Oanchor anchor ->
      let (pageno, y, _) = anchor in
      let y = getanchory
        (if conf.presentation then (pageno, y, 1.0) else anchor)
      in
      addnav ();
      gotoghyll (truncate y)
  | Ouri uri -> gotounder (Ulinkuri uri)
  | Olaunch cmd -> gotounder (Ulaunch cmd)
  | Oremote remote -> gotounder (Uremote remote)
  | Ohistory hist -> gotohist hist
  | Oremotedest remotedest -> gotounder (Uremotedest remotedest)
;;

class outlinesoucebase fetchoutlines = object (self)
    inherit lvsourcebase
    val mutable m_items = E.a
    val mutable m_minfo = E.a
    val mutable m_orig_items = E.a
    val mutable m_orig_minfo = E.a
    val mutable m_narrow_patterns = []
    val mutable m_gen = -1

    method getitemcount = Array.length m_items

    method getitem n =
      let s, n, _ = m_items.(n) in
      (s, n+0)

    method exit ~(uioh:uioh) ~cancel ~active ~(first:int) ~pan :
      uioh option =
      ignore (uioh, first);
      let items, minfo =
        if m_narrow_patterns = []
        then m_orig_items, m_orig_minfo
        else m_items, m_minfo
      in
      m_pan <- pan;
      if not cancel
      then (
        m_items <- items;
        m_minfo <- minfo;
        gotooutline m_items.(active);
       )
      else (
        m_items <- items;
        m_minfo <- minfo;
       );
      None

    method hasaction (_:int) = true

    method greetmsg =
      if Array.length m_items != Array.length m_orig_items
      then
        let s =
          match m_narrow_patterns with
          | one :: [] -> one
          | many -> String.concat "@Uellipsis" (List.rev many)
        in
        "Narrowed to " ^ s ^ " (ctrl-u to restore)"
      else E.s

    method statestr =
      match m_narrow_patterns with
      | [] -> E.s
      | one :: [] -> one
      | head :: _ -> "@Uellipsis" ^ head

    method narrow pattern =
      match Str.regexp_case_fold pattern with
      | (exception _) -> ()
      | re ->
          let rec loop accu minfo n =
            if n = -1
            then (
              m_items <- Array.of_list accu;
              m_minfo <- Array.of_list minfo;
            )
            else
              let (s, _, _) as o = m_items.(n) in
              let accu, minfo =
                match Str.search_forward re s 0 with
                | (exception Not_found) -> accu, minfo
                | first -> o :: accu, (first, Str.match_end ()) :: minfo
              in
              loop accu minfo (n-1)
          in
          loop [] [] (Array.length m_items - 1)

    method! getminfo = m_minfo

    method denarrow =
      m_orig_items <- fetchoutlines ();
      m_minfo <- m_orig_minfo;
      m_items <- m_orig_items

    method add_narrow_pattern pattern =
      m_narrow_patterns <- pattern :: m_narrow_patterns

    method del_narrow_pattern =
      match m_narrow_patterns with
      | _ :: rest -> m_narrow_patterns <- rest
      | [] -> ()

    method renarrow =
      self#denarrow;
      match m_narrow_patterns with
      | pattern :: [] -> self#narrow pattern; pattern
      | list ->
          List.fold_left (fun accu pattern ->
            self#narrow pattern;
            pattern ^ "@Uellipsis" ^ accu) E.s list

    method calcactive (_:anchor) = 0

    method reset anchor items =
      if state.gen != m_gen
      then (
        m_orig_items <- items;
        m_items <- items;
        m_narrow_patterns <- [];
        m_minfo <- E.a;
        m_orig_minfo <- E.a;
        m_gen <- state.gen;
      )
      else (
        if items != m_orig_items
        then (
          m_orig_items <- items;
          if m_narrow_patterns == []
          then m_items <- items;
        )
      );
      let active = self#calcactive anchor in
      m_active <- active;
      m_first <- firstof m_first active
     end
;;

let outlinesource fetchoutlines =
  (object
    inherit outlinesoucebase fetchoutlines
    method! calcactive anchor =
      let rely = getanchory anchor in
      let rec loop n best bestd =
        if n = Array.length m_items
        then best
        else
          let _, _, kind = m_items.(n) in
          match kind with
          | Oanchor anchor ->
              let orely = getanchory anchor in
              let d = abs_float (orely -. rely) in
              if d < bestd
              then loop (n+1) n d
              else loop (n+1) best bestd
          | Onone | Oremote _ | Olaunch _
          | Oremotedest _ | Ouri _ | Ohistory _ ->
              loop (n+1) best bestd
      in
      loop 0 ~-1 max_float
  end)
;;

let enteroutlinemode, enterbookmarkmode, enterhistmode =
  let mkselector sourcetype =
    let fetchoutlines () =
      match sourcetype with
      | `bookmarks -> Array.of_list state.bookmarks
      | `outlines -> state.outlines
      | `history -> genhistoutlines ()
    in
    let source =
      if sourcetype = `history
      then new outlinesoucebase fetchoutlines
      else outlinesource fetchoutlines
    in
    fun errmsg ->
      let outlines = fetchoutlines () in
      if Array.length outlines = 0
      then (
        showtext ' ' errmsg;
       )
      else (
        resetmstate ();
        Wsi.setcursor Wsi.CURSOR_INHERIT;
        let anchor = getanchor () in
        source#reset anchor outlines;
        state.text <- source#greetmsg;
        state.uioh <-
          coe (new outlinelistview ~zebra:(sourcetype=`history) ~source);
        G.postRedisplay "enter selector";
       )
  in
  let mkenter sourcetype errmsg =
    let enter = mkselector sourcetype in
    fun () -> enter errmsg
  in
  (**)mkenter `outlines "document has no outline"
    , mkenter `bookmarks "document has no bookmarks (yet)"
    , mkenter `history "history is empty"
;;

let quickbookmark ?title () =
  match state.layout with
  | [] -> ()
  | l :: _ ->
      let title =
        match title with
        | None ->
            let tm = Unix.localtime (now ()) in
            Printf.sprintf
              "Quick (page %d) (bookmarked at %02d/%02d/%d %02d:%02d)"
              (l.pageno+1)
              tm.Unix.tm_mday
              (tm.Unix.tm_mon+1)
              (tm.Unix.tm_year + 1900)
              tm.Unix.tm_hour
              tm.Unix.tm_min
        | Some title -> title
      in
      state.bookmarks <- (title, 0, Oanchor (getanchor1 l)) :: state.bookmarks
;;

let setautoscrollspeed step goingdown =
  let incr = max 1 ((abs step) / 2) in
  let incr = if goingdown then incr else -incr in
  let astep = boundastep state.winh (step + incr) in
  state.autoscroll <- Some astep;
;;

let canpan () =
  match conf.columns with
  | Csplit _ -> true
  | Csingle _ | Cmulti _ -> state.x != 0.0 || conf.zoom > 1.0
;;

let panbound x = bound x (~-.(float state.w)) (float state.winw);;

let existsinrow pageno (columns, coverA, coverB) p =
  let last = ((pageno - coverA) mod columns) + columns in
  let rec any = function
    | [] -> false
    | l :: rest ->
        if l.pageno = coverA - 1 || l.pageno = state.pagecount - coverB
        then p l
        else (
          if not (p l)
          then (if l.pageno = last then false else any rest)
          else true
        )
  in
  any state.layout
;;

let nextpage () =
  match state.layout with
  | [] ->
      let pageno = page_of_y state.y in
      gotoghyll (truncate (getpagey (pageno+1)))
  | l :: rest ->
      match conf.columns with
      | Csingle _ ->
          if conf.presentation && rest == [] && l.pageh > l.pagey + l.pagevh
          then
            let y = clamp (pgscale state.winh) in
            gotoghyll (truncate y)
          else
            let pageno = min (l.pageno+1) (state.pagecount-1) in
            gotoghyll (truncate (getpagey pageno))
      | Cmulti ((c, _, _) as cl, _) ->
          if conf.presentation
            && (existsinrow l.pageno cl
                   (fun l -> l.pageh > l.pagey + l.pagevh))
          then
            let y = clamp (pgscale state.winh) in
            gotoghyll (truncate y)
          else
            let pageno = min (l.pageno+c) (state.pagecount-1) in
            gotoghyll (truncate (getpagey pageno))
      | Csplit (n, _) ->
          if l.pageno < state.pagecount - 1 || l.pagecol < n - 1
          then
            let pagey, pageh = getpageyh l.pageno in
            let pagey = pagey +. float (pageh * l.pagecol) in
            let ips = if l.pagecol = 0 then 0 else conf.interpagespace in
            gotoghyll (truncate (pagey +. float (pageh + ips)))
;;

let gotoghyll y = gotoghyll (truncate y);;

let prevpage () =
  match state.layout with
  | [] ->
      let pageno = page_of_y state.y in
      gotoghyll (getpagey (pageno-1))
  | l :: _ ->
      match conf.columns with
      | Csingle _ ->
          if conf.presentation && l.pagey != 0
          then
            gotoghyll (clamp (pgscale ~-(state.winh)))
          else
            let pageno = max 0 (l.pageno-1) in
            gotoghyll (getpagey pageno)
      | Cmulti ((c, _, coverB) as cl, _) ->
          if conf.presentation &&
            (existsinrow l.pageno cl (fun l -> l.pagey != 0))
          then
            gotoghyll (clamp (pgscale ~-(state.winh)))
          else
            let decr =
              if l.pageno = state.pagecount - coverB
              then 1
              else c
            in
            let pageno = max 0 (l.pageno-decr) in
            gotoghyll (getpagey pageno)
      | Csplit (n, _) ->
          let y =
            if l.pagecol = 0
            then
              if l.pageno = 0
              then l.pagey
              else
                let pageno = max 0 (l.pageno-1) in
                let pagey, pageh = getpageyh pageno in
                truncate (pagey +. (float ((n-1)*pageh)))
            else
              let pagey, pageh = getpageyh l.pageno in
              truncate
                (pagey +. float (pageh * (l.pagecol-1) - conf.interpagespace))
          in
          gotoghyll (float y)
;;

let save () =
  if emptystr conf.savecmd
  then error "don't know where to save modified document"
  else
    let savecmd = Str.global_replace percentsre state.path conf.savecmd in
    let path =
      getcmdoutput
        (fun s -> error "failed to obtain path to the saved copy: %s" s)
        savecmd
    in
    if nonemptystr path
    then
      let tmp = path ^ ".tmp" in
      savedoc tmp;
      Unix.rename tmp path;
;;

let viewkeyboard key mask =
  let enttext te =
    let mode = state.mode in
    state.mode <- Textentry (te, fun _ -> state.mode <- mode);
    state.text <- E.s;
    enttext ();
    G.postRedisplay "view:enttext"
  in
  let ctrl = Wsi.withctrl mask in
  let key = Wsi.keypadtodigitkey key in
  match key with
  | @Q -> exit 0

  | @W ->
      if hasunsavedchanges ()
      then save ()

  | @insert ->
      if conf.angle mod 360 = 0 && not (isbirdseye state.mode)
      then (
        state.mode <- LinkNav (Ltgendir 0);
        gotoxy state.x state.y;
      )
      else impmsg "keyboard link navigation does not work under rotation"

  | @escape | @q ->
      begin match state.mstate with
      | Mzoomrect _ ->
          resetmstate ();
          G.postRedisplay "kill rect";
      | Msel _
      | Mpan _
      | Mscrolly | Mscrollx
      | Mzoom _
      | Mnone ->
          begin match state.mode with
          | LinkNav _ ->
              state.mode <- View;
              G.postRedisplay "esc leave linknav"
          | Birdseye _
          | Textentry _
          | View ->
              match state.ranchors with
              | [] -> raise Quit
              | (path, password, anchor, origin) :: rest ->
                  state.ranchors <- rest;
                  state.anchor <- anchor;
                  state.origin <- origin;
                  state.nameddest <- E.s;
                  opendoc path password
          end;
      end;

  | @backspace ->
      gotoghyll (getnav ~-1)

  | @o ->
      enteroutlinemode ()

  | @H ->
      enterhistmode ()

  | @u ->
      state.rects <- [];
      state.text <- E.s;
      Hashtbl.iter (fun _ opaque ->
          clearmark opaque;
          Hashtbl.clear state.prects) state.pagemap;
      G.postRedisplay "dehighlight";

  | @slash | @question ->
      let ondone isforw s =
        cbput state.hists.pat s;
        state.searchpattern <- s;
        search s isforw
      in
      let s = String.make 1 (Char.chr key) in
      enttext (s, E.s, Some (onhist state.hists.pat),
              textentry, ondone (key = @slash), true)

  | @plus | @kpplus | @equals when ctrl ->
      let incr = if conf.zoom +. 0.01 > 0.1 then 0.1 else 0.01 in
      pivotzoom (conf.zoom +. incr)

  | @plus | @kpplus ->
      let ondone s =
        let n =
          try int_of_string s with exn ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s @@ exntos exn;
            max_int
        in
        if n != max_int
        then (
          conf.pagebias <- n;
          state.text <- "page bias is now " ^ string_of_int n;
        )
      in
      enttext ("page bias: ", E.s, None, intentry, ondone, true)

  | @minus | @kpminus when ctrl ->
      let decr = if conf.zoom -. 0.1 < 0.1 then 0.01 else 0.1 in
      pivotzoom (max 0.01 (conf.zoom -. decr))

  | @minus | @kpminus ->
      let ondone msg = state.text <- msg in
      enttext (
        "option [acfhilpstvxACFPRSZTISM]: ", E.s, None,
        optentry state.mode, ondone, true
      )

  | @0 when ctrl ->
      if conf.zoom = 1.0
      then gotoxy 0.0 state.y
      else setzoom 1.0

  | (@1 | @2) when ctrl && conf.fitmodel != FitPage -> (* ctrl-1/2 *)
      let cols =
        match conf.columns with
        | Csingle _ | Cmulti _ -> 1
        | Csplit (n, _) -> n
      in
      let h = state.winh -
        conf.interpagespace lsl (if conf.presentation then 1 else 0)
      in
      let zoom = zoomforh state.winw h 0 cols in
      if zoom > 0.0 && (key = @2 || zoom < 1.0)
      then setzoom zoom

  | @3 when ctrl ->
      let fm =
        match conf.fitmodel with
        | FitWidth -> FitProportional
        | FitProportional -> FitPage
        | FitPage -> FitWidth
      in
      state.text <- "fit model: " ^ FMTE.to_string fm;
      reqlayout conf.angle fm

  | @4 when ctrl ->            (* ctrl-4 *)
     let zoom = getmaxw () /. float state.winw in
     if zoom > 0.0 then setzoom zoom

  | @F9 ->
      togglebirdseye ()

  | @9 when ctrl ->
      togglebirdseye ()

  | (48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57)
      when not ctrl ->                  (* 0..9 *)
      let ondone s =
        let n =
          try int_of_string s with exn ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s @@ exntos exn;
            -1
        in
        if n >= 0
        then (
          addnav ();
          cbput state.hists.pag (string_of_int n);
          gotopage1 (n + conf.pagebias - 1) 0;
        )
      in
      let pageentry text key =
        match Char.unsafe_chr key with
        | 'g' -> TEdone text
        | _ -> intentry text key
      in
      let text = String.make 1 (Char.chr key) in
      enttext (":", text, Some (onhist state.hists.pag),
               pageentry, ondone, true)

  | @b ->
      conf.scrollb <- if conf.scrollb = 0 then (scrollbvv lor scrollbhv) else 0;
      reshape state.winw state.winh;

  | @B ->
      state.bzoom <- not state.bzoom;
      state.rects <- [];
      showtext ' ' ("block zoom " ^ if state.bzoom then "on" else "off")

  | @l ->
      conf.hlinks <- not conf.hlinks;
      state.text <- "highlightlinks " ^ if conf.hlinks then "on" else "off";
      G.postRedisplay "toggle highlightlinks";

  | @F ->
      if conf.angle mod 360 = 0
      then (
        state.glinks <- true;
        let mode = state.mode in
        state.mode <-
          Textentry (
              (":", E.s, None, linknentry, linknact gotounder, false),
              (fun _ ->
                state.glinks <- false;
                state.mode <- mode)
            );
        state.text <- E.s;
        G.postRedisplay "view:linkent(F)"
      )
      else impmsg "hint mode does not work under rotation"

  | @y ->
      state.glinks <- true;
      let mode = state.mode in
      state.mode <- Textentry (
        (
          ":", E.s, None, linknentry, linknact (fun under ->
            selstring (undertext under);
          ), false
        ),
        fun _ ->
          state.glinks <- false;
          state.mode <- mode
      );
      state.text <- E.s;
      G.postRedisplay "view:linkent"

  | @a ->
      begin match state.autoscroll with
      | Some step ->
          conf.autoscrollstep <- step;
          state.autoscroll <- None
      | None ->
          if conf.autoscrollstep = 0
          then state.autoscroll <- Some 1
          else state.autoscroll <- Some conf.autoscrollstep
      end

  | @p when ctrl ->
     launchpath ()             (* XXX where do error messages go? *)

  | @P ->
      setpresentationmode (not conf.presentation);
      showtext ' ' ("presentation mode " ^
                       if conf.presentation then "on" else "off");

  | @f ->
      if List.mem Wsi.Fullscreen state.winstate
      then Wsi.reshape conf.cwinw conf.cwinh
      else Wsi.fullscreen ()

  | @p | @N ->
      search state.searchpattern false

  | @n | @F3 ->
      search state.searchpattern true

  | @t ->
      begin match state.layout with
      | [] -> ()
      | l :: _ ->
          gotoghyll (getpagey l.pageno)
      end

  | @space ->
      nextpage ()

  | @delete | @kpdelete ->                  (* delete *)
      prevpage ()

  | @equals ->
      showtext ' ' (describe_location ());

  | @w ->
      begin match state.layout with
      | [] -> ()
      | l :: _ ->
          Wsi.reshape l.pagew l.pageh;
          G.postRedisplay "w"
      end

  | @apos ->
      enterbookmarkmode ()

  | @h | @F1 ->
      enterhelpmode ()

  | @i ->
      enterinfomode ()

  | @e when Buffer.length state.errmsgs > 0 ->
      entermsgsmode ()

  | @m ->
      let ondone s =
        match state.layout with
        | l :: _ ->
            if nonemptystr s
            then
              state.bookmarks <-
                (s, 0, Oanchor (getanchor1 l)) :: state.bookmarks
        | _ -> ()
      in
      enttext ("bookmark: ", E.s, None, textentry, ondone, true)

  | @tilde ->
      quickbookmark ();
      showtext ' ' "Quick bookmark added";

  | @z ->
      begin match state.layout with
      | l :: _ ->
          let rect = getpdimrect l.pagedimno in
          let w, h =
            if conf.crophack
            then
              (truncate (1.8 *. (rect.(1) -. rect.(0))),
              truncate (1.2 *. (rect.(3) -. rect.(0))))
            else
              (truncate (rect.(1) -. rect.(0)),
              truncate (rect.(3) -. rect.(0)))
          in
          let w = truncate ((float w)*.conf.zoom)
          and h = truncate ((float h)*.conf.zoom) in
          if w != 0 && h != 0
          then (
            state.anchor <- getanchor ();
            Wsi.reshape w (h + conf.interpagespace)
          );
          G.postRedisplay "z";

      | [] -> ()
      end

  | @x -> state.roam ()

  | @Lt | @Gt ->
      reqlayout (conf.angle +
                   (if key = @Gt then 30 else -30)) conf.fitmodel

  | @Lb | @Rb ->
      conf.colorscale <-
        bound (conf.colorscale +. (if key = 93 then 0.1 else -0.1)) 0.0 1.0
      ;
      G.postRedisplay "brightness";

  | @c when state.mode = View ->
      if Wsi.withalt mask
      then (
        if conf.zoom > 1.0
        then
          let m = (state.winw - state.w) / 2 in
          gotoxy_and_clear_text (float m) state.y
      )
      else
        let (c, a, b), z =
          match state.prevcolumns with
          | None -> (1, 0, 0), 1.0
          | Some (columns, z) ->
              let cab =
                match columns with
                | Csplit (c, _) -> -c, 0, 0
                | Cmulti ((c, a, b), _) -> c, a, b
                | Csingle _ -> 1, 0, 0
              in
              cab, z
        in
        setcolumns View c a b;
        setzoom z

  | @down | @up when ctrl && Wsi.withshift mask ->
      let zoom, x = state.prevzoom in
      setzoom zoom;
      state.x <- float x;

  | @k | @up | @kpup ->
      begin match state.autoscroll with
      | None ->
          begin match state.mode with
          | Birdseye beye -> upbirdseye 1 beye
          | Textentry _
          | View
          | LinkNav _ ->
              if ctrl
              then gotoxy_and_clear_text state.x (clamp ~-(state.winh/2))
              else (
                if not (Wsi.withshift mask) && conf.presentation
                then prevpage ()
                else gotoghyll1 true (truncate (clamp (-conf.scrollstep)))
              )
          end
      | Some n ->
          setautoscrollspeed n false
      end

  | @j | @down | @kpdown ->
      begin match state.autoscroll with
      | None ->
          begin match state.mode with
          | Birdseye beye -> downbirdseye 1 beye
          | Textentry _
          | View
          | LinkNav _ ->
              if ctrl
              then gotoxy_and_clear_text state.x (clamp (state.winh/2))
              else (
                if not (Wsi.withshift mask) && conf.presentation
                then nextpage ()
                else gotoghyll1 true (truncate (clamp (conf.scrollstep)))
              )
          end
      | Some n ->
          setautoscrollspeed n true
      end

  | @left | @right | @kpleft | @kpright when not (Wsi.withalt mask) ->
      if canpan ()
      then
        let dx =
          if ctrl
          then state.winw / 2
          else conf.hscrollstep
        in
        let dx = if key = @left || key = @kpleft then dx else -dx in
        gotoxy_and_clear_text (panbound (state.x +. float dx)) state.y
      else (
        state.text <- E.s;
        G.postRedisplay "left/right"
      )

  | @prior | @kpprior ->
      let y =
        if ctrl
        then
          match state.layout with
          | [] -> state.y
          | l :: _ -> state.y -. float l.pagey
        else
          clamp (pgscale (-state.winh))
      in
      gotoghyll y

  | @next | @kpnext ->
      let y =
        if ctrl
        then
          match List.rev state.layout with
          | [] -> state.y
          | l :: _ -> getpagey l.pageno
        else
          clamp (pgscale state.winh)
      in
      gotoghyll y

  | @g | @home | @kphome ->
      addnav ();
      gotoghyll 0.0
  | @G | @jend | @kpend ->
      addnav ();
      gotoghyll (clamp (truncate state.maxy))

  | @right | @kpright when Wsi.withalt mask ->
      gotoghyll (getnav 1)
  | @left | @kpleft when Wsi.withalt mask ->
      gotoghyll (getnav ~-1)

  | @r ->
      reload ()

  | @v when conf.debug ->
      state.rects <- [];
      List.iter (fun l ->
        match getopaque l.pageno with
        | None -> ()
        | Some opaque ->
            let x0, y0, x1, y1 = pagebbox opaque in
            let rect = (float x0, float y0,
                        float x1, float y0,
                        float x1, float y1,
                        float x0, float y1) in
            debugrect rect;
            let color = (0.0, 0.0, 1.0 /. (l.pageno mod 3 |> float), 0.5) in
            state.rects <- (l.pageno, color, rect) :: state.rects;
      ) state.layout;
      G.postRedisplay "v";

  | @pipe ->
      let mode = state.mode in
      let cmd = ref E.s in
      let onleave = function
        | Cancel -> state.mode <- mode
        | Confirm ->
            List.iter (fun l ->
              match getopaque l.pageno with
              | Some opaque -> pipesel opaque !cmd
              | None -> ()) state.layout;
            state.mode <- mode
      in
      let ondone s =
        cbput state.hists.sel s;
        cmd := s
      in
      let te =
        "| ", !cmd, Some (onhist state.hists.sel), textentry, ondone, true
      in
      G.postRedisplay "|";
      state.mode <- Textentry (te, onleave);

  | _ ->
      vlog "huh? %s" (Wsi.keyname key)
;;

let linknavkeyboard key mask linknav =
  let getpage pageno =
    let rec loop = function
      | [] -> None
      | l :: _ when l.pageno = pageno -> Some l
      | _ :: rest -> loop rest
    in loop state.layout
  in
  let doexact (pageno, n) =
    match getopaque pageno, getpage pageno with
    | Some opaque, Some l ->
        if key = @enter || key = @kpenter
        then
          let under = getlink opaque n in
          G.postRedisplay "link gotounder";
          gotounder under;
          state.mode <- View;
        else
          let opt, dir =
            match key with
            | @home ->
                Some (findlink opaque LDfirst), -1

            | @jend ->
                Some (findlink opaque LDlast), 1

            | @left ->
                Some (findlink opaque (LDleft n)), -1

            | @right ->
                Some (findlink opaque (LDright n)), 1

            | @up ->
                Some (findlink opaque (LDup n)), -1

            | @down ->
                Some (findlink opaque (LDdown n)), 1

            | _ -> None, 0
          in
          let pwl l dir =
            begin match findpwl l.pageno dir with
            | Pwlnotfound -> ()
            | Pwl pageno ->
                let notfound dir =
                  state.mode <- LinkNav (Ltgendir dir);
                  let y, h = getpageyh pageno in
                  let y =
                    if dir < 0
                    then y +. (float (h - state.winh))
                    else y
                  in
                  gotoxy state.x y
                in
                begin match getopaque pageno, getpage pageno with
                | Some opaque, Some _ ->
                    let link =
                      let ld = if dir > 0 then LDfirst else LDlast in
                      findlink opaque ld
                    in
                    begin match link with
                    | Lfound m ->
                        showlinktype (getlink opaque m);
                        state.mode <- LinkNav (Ltexact (pageno, m));
                        G.postRedisplay "linknav jpage";
                    | Lnotfound -> notfound dir
                    end;
                | _ -> notfound dir
                end;
            end;
          in
          begin match opt with
          | Some Lnotfound -> pwl l dir;
          | Some (Lfound m) ->
              if m = n
              then pwl l dir
              else (
                let _, y0, _, y1 = getlinkrect opaque m in
                if y0 < l.pagey
                then gotopage1 l.pageno y0
                else (
                  let d = fstate.fontsize + 1 in
                  if y1 - l.pagey > l.pagevh - d
                  then gotopage1 l.pageno (y1 - state.winh + d)
                  else G.postRedisplay "linknav";
                );
                showlinktype (getlink opaque m);
                state.mode <- LinkNav (Ltexact (l.pageno, m));
              )

          | None -> viewkeyboard key mask
          end;
    | _ -> viewkeyboard key mask
  in
  if key = @insert
  then (
    state.mode <- View;
    G.postRedisplay "leave linknav"
  )
  else
    match linknav with
    | Ltgendir _ | Ltnotready _ -> viewkeyboard key mask
    | Ltexact exact -> doexact exact
;;

let keyboard key mask =
  if (key = @g && Wsi.withctrl mask) && not (istextentry state.mode)
  then wcmd "interrupt"
  else state.uioh <- state.uioh#key key mask
;;

let birdseyekeyboard key mask
    ((oconf, leftx, pageno, hooverpageno, anchor) as beye) =
  let incr =
    match conf.columns with
    | Csingle _ -> 1
    | Cmulti ((c, _, _), _) -> c
    | Csplit _ -> failwith "bird's eye split mode"
  in
  let pgh layout = List.fold_left
      (fun m l -> max l.pageh m) state.winh layout in
  match key with
  | @l when Wsi.withctrl mask ->
      let y, h = getpageyh pageno in
      let top = (state.winh - h) / 2 in
      gotoxy state.x (max 0.0 (y -. float top))
  | @enter | @kpenter -> leavebirdseye beye false
  | @escape -> leavebirdseye beye true
  | @up -> upbirdseye incr beye
  | @down -> downbirdseye incr beye
  | @left -> upbirdseye 1 beye
  | @right -> downbirdseye 1 beye

  | @prior ->
      begin match state.layout with
      | l :: _ ->
          if l.pagey != 0
          then (
            state.mode <- Birdseye (
              oconf, leftx, l.pageno, hooverpageno, anchor
            );
            gotopage1 l.pageno 0;
          )
          else (
            let layout = layout (truncate state.x)
                                (state.y -. float state.winh)
                                state.winw
                                (pgh state.layout) in
            match layout with
            | [] -> gotoxy state.x (clamp (-state.winh))
            | l :: _ ->
                state.mode <- Birdseye (
                  oconf, leftx, l.pageno, hooverpageno, anchor
                );
                gotopage1 l.pageno 0
          );

      | [] -> gotoxy state.x (clamp (-state.winh))
      end;

  | @next ->
      begin match List.rev state.layout with
      | l :: _ ->
         let layout = layout (truncate state.x)
                             (state.y +. (float (pgh state.layout)))
                             state.winw state.winh in
         begin match layout with
          | [] ->
              let incr = l.pageh - l.pagevh in
              if incr = 0
              then (
                state.mode <-
                  Birdseye (
                    oconf, leftx, state.pagecount - 1, hooverpageno, anchor
                  );
                G.postRedisplay "birdseye pagedown";
              )
              else gotoxy state.x (clamp (incr + conf.interpagespace*2));

          | l :: _ ->
              state.mode <-
                Birdseye (oconf, leftx, l.pageno, hooverpageno, anchor);
              gotopage1 l.pageno 0;
          end

      | [] -> gotoxy state.x (clamp state.winh)
      end;

  | @home ->
      state.mode <- Birdseye (oconf, leftx, 0, hooverpageno, anchor);
      gotopage1 0 0

  | @jend ->
      let pageno = state.pagecount - 1 in
      state.mode <- Birdseye (oconf, leftx, pageno, hooverpageno, anchor);
      if not (pagevisible state.layout pageno)
      then
        let h =
          match List.rev state.pdims with
          | [] -> state.winh
          | (_, _, h, _) :: _ -> h
        in
        gotoxy
          state.x
          (max 0.0 (getpagey pageno -.
                      float (state.winh - h - conf.interpagespace)))
      else G.postRedisplay "birdseye end";

  | _ -> viewkeyboard key mask
;;

let drawpage l =
  let color =
    match state.mode with
    | Textentry _ -> scalecolor 0.4
    | LinkNav _
    | View -> scalecolor 1.0
    | Birdseye (_, _, pageno, hooverpageno, _) ->
        if l.pageno = hooverpageno
        then scalecolor 0.9
        else (
          if l.pageno = pageno
          then (
            let c = scalecolor 1.0 in
            GlDraw.color c;
            GlDraw.line_width 3.0;
            let dispx = l.pagedispx in
            linerect
              (float (dispx-1)) (float (l.pagedispy-1))
              (float (dispx+l.pagevw+1))
              (float (l.pagedispy+l.pagevh+1))
              ;
            GlDraw.line_width 1.0;
            c;
           )
          else scalecolor 0.8
        )
  in
  drawtiles l color;
;;

let postdrawpage l linkindexbase =
  match getopaque l.pageno with
  | Some opaque ->
      if tileready l l.pagex l.pagey
      then
        let x = l.pagedispx - l.pagex
        and y = l.pagedispy - l.pagey in
        let hlmask =
          match conf.columns with
          | Csingle _ | Cmulti _ ->
              (if conf.hlinks then 1 else 0)
              + (if state.glinks
                  && not (isbirdseye state.mode) then 2 else 0)
          | Csplit _ -> 0
        in
        let s =
          match state.mode with
          | Textentry ((_, s, _, _, _, _), _) when state.glinks -> s
          | Textentry _
          | Birdseye _
          | View
          | LinkNav _ -> E.s
        in
        Hashtbl.find_all state.prects l.pageno |>
          List.iter (fun vals -> drawprect opaque x y vals);
        let n = postprocess opaque hlmask x y (linkindexbase, s, conf.hfsize) in
        if n < 0
        then (state.redisplay <- true; 0)
        else n
      else 0
  | _ -> 0
;;

let scrollindicator () =
  let sbw, ph, sh = state.uioh#scrollph in
  let sbh, pw, sw = state.uioh#scrollpw in

  let x0,x1,hx0 =
    if conf.leftscroll
    then (0, sbw, sbw)
    else ((state.winw - sbw), state.winw, 0)
  in

  Gl.enable `blend;
  GlDraw.color (0.64, 0.64, 0.64) ~alpha:0.7;
  filledrect (float x0) 0. (float x1) (float state.winh);
  filledrect
    (float hx0) (float (state.winh - sbh))
    (float (hx0 + state.winw)) (float state.winh)
  ;
  GlDraw.color (0.0, 0.0, 0.0) ~alpha:0.7;

  filledrect (float x0) ph (float x1) (ph +. sh);
  let pw = pw +. float hx0 in
  filledrect pw (float (state.winh - sbh)) (pw +. sw) (float state.winh);
  Gl.disable `blend;
;;

let showsel () =
  match state.mstate with
  | Mnone | Mscrolly | Mscrollx | Mpan _ | Mzoom _ | Mzoomrect _ ->
      ()

  | Msel ((x0, y0), (x1, y1)) ->
      let identify opaque l px py = Some (opaque, l.pageno, px, py) in
      let o0,n0,px0,py0 = onppundermouse identify x0 y0 (~< E.s, -1, 0, 0) in
      let _o1,n1,px1,py1 = onppundermouse identify x1 y1 (~< E.s, -1, 0, 0) in
      if n0 != -1 && n0 = n1 then seltext o0 (px0, py0, px1, py1);
;;

let showrects = function [] -> () | rects ->
  Gl.enable `blend;
  GlDraw.color (0.0, 0.0, 1.0) ~alpha:0.5;
  GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
  List.iter
    (fun (pageno, c, (x0, y0, x1, y1, x2, y2, x3, y3)) ->
      List.iter (fun l ->
        if l.pageno = pageno
        then (
          let dx = float (l.pagedispx - l.pagex) in
          let dy = float (l.pagedispy - l.pagey) in
          let r, g, b, alpha = c in
          GlDraw.color (r, g, b) ~alpha;
          filledrect2 (x0+.dx) (y0+.dy)
                      (x1+.dx) (y1+.dy)
                      (x3+.dx) (y3+.dy)
                      (x2+.dx) (y2+.dy);
        )
      ) state.layout
    ) rects
  ;
  Gl.disable `blend;
;;

let display () =
  begin match conf.columns, state.layout with
  | Csingle _, _ :: _ ->
     GlDraw.color (scalecolor2 conf.bgcolor);
     let y =
       List.fold_left (fun y l ->
           let x0 = 0 in
           let y0 = y in
           let x1 = l.pagedispx in
           let y1 = (l.pagedispy + l.pagevh) in
           filledrect (float x0) (float y0) (float x1) (float y1);
           let x0 = x1 + l.pagevw in
           let x1 = state.winw in
           filledrect1 (float x0) (float y0) (float x1) (float y1);
           if y != l.pagedispy
           then (
             let x0 = 0
             and x1 = state.winw in
             let y0 = y
             and y1 = l.pagedispy in
             filledrect1 (float x0) (float y0) (float x1) (float y1);
           );
           l.pagedispy + l.pagevh) 0 state.layout
     in
     let x0 = 0
     and x1 = state.winw in
     let y0 = y
     and y1 = state.winh in
     filledrect1 (float x0) (float y0) (float x1) (float y1)
  | (Cmulti _ | Csplit _), _ | Csingle _, [] ->
     GlClear.color (scalecolor2 conf.bgcolor);
     GlClear.clear [`color];
  end;
  List.iter drawpage state.layout;
  let rects =
    match state.mode with
    | LinkNav (Ltexact (pageno, linkno)) ->
        begin match getopaque pageno with
        | Some opaque ->
            let x0, y0, x1, y1 = getlinkrect opaque linkno in
            let color = (0.0, 0.0, 0.5, 0.5) in
            (pageno, color, (
              float x0, float y0,
              float x1, float y0,
              float x1, float y1,
              float x0, float y1)
            ) :: state.rects
        | None -> state.rects
        end
    | LinkNav (Ltgendir _) | LinkNav (Ltnotready _)
    | Birdseye _
    | Textentry _
    | View -> state.rects
  in
  showrects rects;
  let rec postloop linkindexbase = function
    | l :: rest ->
        let linkindexbase = linkindexbase + postdrawpage l linkindexbase in
        postloop linkindexbase rest
    | [] -> ()
  in
  showsel ();
  postloop 0 state.layout;
  state.uioh#display;
  begin match state.mstate with
  | Mzoomrect ((x0, y0), (x1, y1)) ->
      Gl.enable `blend;
      GlDraw.color (0.3, 0.3, 0.3) ~alpha:0.5;
      GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
      filledrect (float x0) (float y0) (float x1) (float y1);
      Gl.disable `blend;
  | Msel _
  | Mpan _
  | Mscrolly | Mscrollx
  | Mzoom _
  | Mnone -> ()
  end;
  enttext ();
  scrollindicator ();
  Wsi.swapb ();
;;

let zoomrect x y x1 y1 =
  let x0 = min x x1
  and x1 = max x x1
  and y0 = min y y1 in
  let zoom = (float state.w) /. float (x1 - x0) in
  let margin =
    let simple () = if state.w < state.winw then state.w / 2 else 0 in
    match conf.fitmodel with
    | FitWidth | FitProportional -> simple ()
    | FitPage ->
        match conf.columns with
        | Csplit _ ->
            onppundermouse (fun _ l _ _ -> Some l.pagedispx) x0 y0 x0
        | Cmulti _ | Csingle _ -> simple ()
  in
  gotoxy ((state.x +. float margin) -. float x0)
         (state.y +. float y0);
  state.anchor <- getanchor ();
  setzoom zoom;
  resetmstate ();
;;

let annot inline x y =
  match unproject x y with
  | Some (opaque, n, ux, uy) ->
     let add text =
       addannot opaque ux uy text;
       wcmd "freepage %s" (~> opaque);
       Hashtbl.remove state.pagemap (n, state.gen);
       flushtiles ();
       gotoxy state.x state.y
     in
     if inline
     then
       let ondone s = add s in
       let mode = state.mode in
       state.mode <- Textentry (
                         ("annotation: ", E.s, None, textentry, ondone, true),
                         fun _ -> state.mode <- mode);
       state.text <- E.s;
       enttext ();
       G.postRedisplay "annot"
     else
       add @@ getusertext E.s
  | _ -> ()
;;

let zoomblock x y =
  let g opaque l px py =
    match rectofblock opaque px py with
    | Some a ->
        let x0 = a.(0) -. 20. in
        let x1 = a.(1) +. 20. in
        let y0 = a.(2) -. 20. in
        let zoom = (float state.w) /. (x1 -. x0) in
        let pagey = getpagey l.pageno in
        let margin = (state.w - l.pagew)/2 in
        let nx = ~-.x0 -. float margin in
        gotoxy_and_clear_text nx (pagey +. y0);
        state.anchor <- getanchor ();
        setzoom zoom;
        None
    | None -> None
  in
  match conf.columns with
  | Csplit _ ->
     impmsg "block zooming does not work properly in split columns mode"
  | Cmulti _ | Csingle _ -> onppundermouse g x y ()
;;

let scrollx x =
  let winw = state.winw - 1 in
  let s = float x /. float winw in
  let destx = float (state.w + winw) *. s in
  gotoxy_and_clear_text (float winw -. destx) state.y;
  state.mstate <- Mscrollx;
;;

let scrolly y =
  let s = float y /. float state.winh in
  let desty =
    state.maxy -. (if conf.maxhfit then float state.winh else 0.0) *. s in
  gotoxy_and_clear_text state.x desty;
  state.mstate <- Mscrolly;
;;

let viewmulticlick clicks x y mask =
  let g opaque l px py =
    let mark =
      match clicks with
      | 2 -> Mark_word
      | 3 -> Mark_line
      | 4 -> Mark_block
      | _ -> Mark_page
    in
    if markunder opaque px py mark
    then (
      Some (fun () ->
        let dopipe cmd =
          match getopaque l.pageno with
          | None -> ()
          | Some opaque -> pipesel opaque cmd
        in
        state.roam <- (fun () -> dopipe conf.paxcmd);
        if not (Wsi.withctrl mask) then dopipe conf.selcmd;
      )
    )
    else None
  in
  G.postRedisplay "viewmulticlick";
  onppundermouse g x y (fun () -> impmsg "nothing to select") ();
;;

let canselect () =
  match conf.columns with
  | Csplit _ -> false
  | Csingle _ | Cmulti _ -> conf.angle mod 360 = 0
;;

let viewmouse button down x y mask =
  match button with
  | n when (n == 4 || n == 5) && not down ->
      if Wsi.withctrl mask
      then (
        match state.mstate with
        | Mzoom (oldn, i, (ftx, fty)) ->
           let recenter =
             if false
             then abs (ftx - x) > 5 || abs (fty - y) > 5
             else false
           in
            if oldn = n
            then (
              if i = 2
              then
                let incr =
                  match n with
                  | 5 ->
                      if conf.zoom +. 0.01 > 0.1 then 0.1 else 0.01
                  | _ ->
                      if conf.zoom -. 0.1 < 0.1 then -0.01 else -0.1
                in
                let zoom = conf.zoom -. incr in
                if recenter
                then pivotzoom ~x:(float x) ~y:(float y) zoom
                else pivotzoom zoom;
                state.mstate <- Mzoom (n, 0, (x, y));
              else
                state.mstate <- Mzoom (n, i+1, (ftx, fty));
            )
            else state.mstate <- Mzoom (n, 0, (ftx, fty))

        | Msel _
        | Mpan _
        | Mscrolly | Mscrollx
        | Mzoomrect _
        | Mnone -> state.mstate <- Mzoom (n, 0, (0, 0))
      )
      else (
        match state.autoscroll with
        | Some step -> setautoscrollspeed step (n=4)
        | None ->
            if conf.wheelbypage || conf.presentation
            then (
              if n = 4
              then prevpage ()
              else nextpage ()
            )
            else
              let incr =
                if n = 4
                then -conf.scrollstep
                else conf.scrollstep
              in
              let incr = incr * 2 in
              let y = clamp incr in
              gotoxy_and_clear_text state.x y
      )

  | n when (n = 6 || n = 7) && not down && canpan () ->
     let x = panbound (
                 state.x +. (if n = 7 then -2. else 2.) *. float conf.hscrollstep
               ) in
     gotoxy_and_clear_text x state.y

  | 1 when Wsi.withshift mask ->
      state.mstate <- Mnone;
      if not down
      then (
        match unproject x y with
        | None -> ()
        | Some (_, pageno, ux, uy) ->
            let cmd = Printf.sprintf
              "%s %s %d %d %d"
              conf.stcmd state.path pageno ux uy
            in
            match spawn cmd [] with
            | (exception exn) ->
               impmsg "execution of synctex command(%S) failed: %S"
                      conf.stcmd @@ exntos exn
            | _pid -> ()
      )

  | 1 when Wsi.withctrl mask ->
      if down
      then (
        Wsi.setcursor Wsi.CURSOR_FLEUR;
        state.mstate <- Mpan (x, y)
      )
      else
        state.mstate <- Mnone

  | 3 ->
      if down
      then (
        if Wsi.withshift mask
        then (
          annot conf.annotinline x y;
          G.postRedisplay "addannot"
        )
        else
          let p = (x, y) in
          Wsi.setcursor Wsi.CURSOR_CYCLE;
          state.mstate <- Mzoomrect (p, p)
      )
      else (
        match state.mstate with
        | Mzoomrect ((x0, y0), _) ->
            if abs (x-x0) > 10 && abs (y - y0) > 10
            then zoomrect x0 y0 x y
            else (
              resetmstate ();
              G.postRedisplay "kill accidental zoom rect";
            )
        | Msel _
        | Mpan _
        | Mscrolly | Mscrollx
        | Mzoom _
        | Mnone ->
            resetmstate ()
      )

  | 1 when vscrollhit x ->
      if down
      then
        let _, position, sh = state.uioh#scrollph in
        if y > truncate position && y < truncate (position +. sh)
        then state.mstate <- Mscrolly
        else scrolly y
      else
        state.mstate <- Mnone

  | 1 when y > state.winh - hscrollh () ->
      if down
      then
        let _, position, sw = state.uioh#scrollpw in
        if x > truncate position && x < truncate (position +. sw)
        then state.mstate <- Mscrollx
        else scrollx x
      else
        state.mstate <- Mnone

  | 1 when state.bzoom -> if not down then zoomblock x y

  | 1 ->
      let dest = if down then getunder x y else Unone in
      begin match dest with
      | Ulinkgoto _
      | Ulinkuri _
      | Uremote _ | Uremotedest _
      | Uunexpected _ | Ulaunch _ | Unamed _ ->
          gotounder dest

      | Unone when down ->
          Wsi.setcursor Wsi.CURSOR_FLEUR;
          state.mstate <- Mpan (x, y);

      | Uannotation (opaque, slinkindex) -> enterannotmode opaque slinkindex

      | Unone | Utext _ ->
          if down
          then (
            if canselect ()
            then (
              state.mstate <- Msel ((x, y), (x, y));
              G.postRedisplay "mouse select";
            )
          )
          else (
            match state.mstate with
            | Mnone -> ()

            | Mzoom _ | Mscrollx | Mscrolly ->
                state.mstate <- Mnone

            | Mzoomrect ((x0, y0), _) ->
                zoomrect x0 y0 x y

            | Mpan _ ->
                Wsi.setcursor Wsi.CURSOR_INHERIT;
                state.mstate <- Mnone

            | Msel ((x0, y0), (x1, y1)) ->
                let rec loop = function
                  | [] -> ()
                  | l :: rest ->
                      let inside =
                        let a0 = l.pagedispy in
                        let a1 = a0 + l.pagevh in
                        let b0 = l.pagedispx in
                        let b1 = b0 + l.pagevw in
                        ((y0 >= a0 && y0 <= a1) || (y1 >= a0 && y1 <= a1))
                        && ((x0 >= b0 && x0 <= b1) || (x1 >= b0 && x1 <= b1))
                      in
                      if inside
                      then
                        match getopaque l.pageno with
                        | Some opaque ->
                            let dosel cmd () =
                              match Unix.pipe () with
                              | (exception exn) ->
                                 impmsg "cannot create sel pipe: %s" @@
                                   exntos exn;
                              | (r, w) ->
                                  let clo what fd =
                                    Ne.clo fd (fun msg ->
                                      dolog "%s close failed: %s" what msg)
                                  in
                                  let pid =
                                    try spawn cmd [r, 0; w, -1]
                                    with exn ->
                                      dolog "cannot execute %S: %s"
                                            cmd @@ exntos exn;
                                      -1
                                  in
                                  if pid > 0
                                  then (
                                    copysel w opaque;
                                    G.postRedisplay "copysel";
                                  )
                                  else clo "Msel pipe/w" w;
                                  clo "Msel pipe/r" r;
                            in
                            dosel conf.selcmd ();
                            state.roam <- dosel conf.paxcmd;
                        | None -> ()
                      else loop rest
                in
                loop state.layout;
                resetmstate ();
          )
      end

  | _ -> ()
;;

let birdseyemouse button down x y mask
    (conf, leftx, _, hooverpageno, anchor) =
  match button with
  | 1 when down ->
      let rec loop = function
        | [] -> ()
        | l :: rest ->
            if y > l.pagedispy && y < l.pagedispy + l.pagevh
              && x > l.pagedispx && x < l.pagedispx + l.pagevw
            then (
              leavebirdseye (conf, leftx, l.pageno, hooverpageno, anchor) false;
            )
            else loop rest
      in
      loop state.layout
  | 3 -> ()
  | _ -> viewmouse button down x y mask
;;

let uioh = object
  method display = ()

  method key key mask =
    begin match state.mode with
    | Textentry textentry -> textentrykeyboard key mask textentry
    | Birdseye birdseye -> birdseyekeyboard key mask birdseye
    | View -> viewkeyboard key mask
    | LinkNav linknav -> linknavkeyboard key mask linknav
    end;
    state.uioh

  method button button bstate x y mask =
    begin match state.mode with
    | LinkNav _
    | View -> viewmouse button bstate x y mask
    | Birdseye beye -> birdseyemouse button bstate x y mask beye
    | Textentry _ -> ()
    end;
    state.uioh

  method multiclick clicks x y mask =
    begin match state.mode with
    | LinkNav _
    | View -> viewmulticlick clicks x y mask
    | Birdseye _
    | Textentry _ -> ()
    end;
    state.uioh

  method motion x y =
    begin match state.mode with
    | Textentry _ -> ()
    | View | Birdseye _ | LinkNav _ ->
        match state.mstate with
        | Mzoom _ | Mnone -> ()

        | Mpan (x0, y0) ->
            let dx = x - x0
            and dy = y0 - y in
            state.mstate <- Mpan (x, y);
            let x = if canpan ()
                    then panbound (state.x +. float dx)
                    else state.x in
            let y = clamp dy in
            gotoxy_and_clear_text x y

        | Msel (a, _) ->
            state.mstate <- Msel (a, (x, y));
            G.postRedisplay "motion select";

        | Mscrolly ->
            let y = min state.winh (max 0 y) in
            scrolly y

        | Mscrollx ->
            let x = min state.winw (max 0 x) in
            scrollx x

        | Mzoomrect (p0, _) ->
            state.mstate <- Mzoomrect (p0, (x, y));
            G.postRedisplay "motion zoomrect";
    end;
    state.uioh

  method pmotion x y =
    begin match state.mode with
    | Birdseye (conf, leftx, pageno, hooverpageno, anchor) ->
        let rec loop = function
          | [] ->
              if hooverpageno != -1
              then (
                state.mode <- Birdseye (conf, leftx, pageno, -1, anchor);
                G.postRedisplay "pmotion birdseye no hoover";
              )
          | l :: rest ->
              if y > l.pagedispy && y < l.pagedispy + l.pagevh
                && x > l.pagedispx && x < l.pagedispx + l.pagevw
              then (
                state.mode <- Birdseye (conf, leftx, pageno, l.pageno, anchor);
                G.postRedisplay "pmotion birdseye hoover";
              )
              else loop rest
        in
        loop state.layout

    | Textentry _ -> ()

    | LinkNav _
    | View ->
        match state.mstate with
        | Mpan _ | Msel _ | Mzoom _ | Mscrolly | Mscrollx | Mzoomrect _ -> ()
        | Mnone ->
            updateunder x y;
            if canselect ()
            then
              match conf.pax with
              | None -> ()
              | Some r ->
                  let past, _, _ = !r in
                  let now = now () in
                  let delta = now -. past in
                  if delta > 0.01
                  then paxunder x y
                  else r := (now, x, y)
    end;
    state.uioh

  method infochanged _ = ()

  method scrollph =
    let maxy = state.maxy -. (if conf.maxhfit then float state.winh else 0.0) in
    let p, h =
      if maxy = 0.0
      then 0.0, float state.winh
      else scrollph (truncate state.y) (truncate maxy)
    in
    vscrollw (), p, h

  method scrollpw =
    let fwinw = float (state.winw - vscrollw ()) in
    let sw =
      let sw = fwinw /. float state.w in
      let sw = fwinw *. sw in
      max sw (float conf.scrollh)
    in
    let position =
      let maxx = state.w + state.winw in
      let x = float state.winw -. state.x in
      let percent = x /. float maxx in
      (fwinw -. sw) *. percent
    in
    hscrollh (), position, sw

  method modehash =
    let modename =
      match state.mode with
      | LinkNav _ -> "links"
      | Textentry _ -> "textentry"
      | Birdseye _ -> "birdseye"
      | View -> "view"
    in
    findkeyhash conf modename

  method eformsgs = true
  method alwaysscrolly = false
end;;

let addrect pageno r g b a x0 y0 x1 y1 =
  Hashtbl.add state.prects pageno [|r; g; b; a; x0; y0; x1; y1|];
;;

let ract cmds =
  let cl = splitatspace cmds in
  let scan s fmt f =
    try Scanf.sscanf s fmt f
    with exn ->
      adderrfmt "remote exec"
        "error processing '%S': %s\n" cmds @@ exntos exn
  in
  let rectx s pageno (r, g, b, a) x0 y0 x1 y1 =
    vlog "%s page %d color (%f %f %f %f) x0,y0,x1,y1 = %f %f %f %f"
         s pageno r g b a x0 y0 x1 y1;
    onpagerect
      pageno
      (fun w h ->
        let _,w1,h1,_ = getpagedim pageno in
        let sw = float w1 /. float w
        and sh = float h1 /. float h in
        let x0s = x0 *. sw
        and x1s = x1 *. sw
        and y0s = y0 *. sh
        and y1s = y1 *. sh in
        let rect = (x0s,y0s,x1s,y0s,x1s,y1s,x0s,y1s) in
        let color = (r, g, b, a) in
        if conf.verbose then debugrect rect;
        state.rects <- (pageno, color, rect) :: state.rects;
        G.postRedisplay s;
      )
  in
  match cl with
  | "reload", "" -> reload ()
  | "goto", args ->
      scan args "%u %f %f"
        (fun pageno x y ->
          let cmd, _ = state.geomcmds in
          if emptystr cmd
          then gotopagexy !wtmode pageno x y
          else
            let f prevf () =
              gotopagexy !wtmode pageno x y;
              prevf ()
            in
            state.reprf <- f state.reprf
        )
  | "goto1", args -> scan args "%u %f" gotopage
  | "gotor", args ->
      scan args "%S %u"
        (fun filename pageno -> gotounder (Uremote (filename, pageno)))
  | "gotord", args ->
      scan args "%S %S"
        (fun filename dest -> gotounder (Uremotedest (filename, dest)))
  | "rect", args ->
     scan args "%u %u %f %f %f %f"
          (fun pageno c x0 y0 x1 y1 ->
            let color = (0.0, 0.0, 1.0 /. float c, 0.5) in
            rectx "rect" pageno color x0 y0 x1 y1;
          )
  | "prect", args ->
     scan args "%u %f %f %f %f %f %f %f %f"
          (fun pageno r g b alpha x0 y0 x1 y1 ->
            addrect pageno r g b alpha x0 y0 x1 y1;
            G.postRedisplay "prect"
          )
  | "pgoto", args ->
     scan args "%u %f %f"
          (fun pageno x y ->
            let optopaque =
              match getopaque pageno with
              | Some opaque -> opaque
              | None -> ~< E.s
            in
            pgoto optopaque pageno x y;
            let rec fixx = function
              | [] -> ()
              | l :: rest ->
                 if l.pageno = pageno
                 then gotoxy (state.x -. float l.pagedispx) state.y
                 else fixx rest
            in
            let layout =
              let mult =
                match conf.columns with
                | Csingle _ | Csplit _ -> 1
                | Cmulti ((n, _, _), _) -> n
              in
              layout 0 state.y (state.winw * mult) state.winh
            in
            fixx layout
          )
  | "activatewin", "" -> Wsi.activatewin ()
  | "quit", "" -> raise Quit
  | "keys", keys ->
     begin try
         let l = Config.keys_of_string keys in
         List.iter (fun (k, m) -> keyboard k m) l
       with exn ->
         adderrfmt "error processing keys" "`%S': %s\n" cmds @@ exntos exn
     end
  | "clearrects", "" ->
     Hashtbl.clear state.prects;
     G.postRedisplay "clearrects"
  | _ ->
      adderrfmt "remote command"
        "error processing remote command: %S\n" cmds;
;;

let remote =
  let scratch = Bytes.create 80 in
  let buf = Buffer.create 80 in
  fun fd ->
    match tempfailureretry (Unix.read fd scratch 0) 80 with
    | (exception Unix.Unix_error (Unix.EAGAIN, _, _)) -> None
    | 0 ->
        Unix.close fd;
        if Buffer.length buf > 0
        then (
          let s = Buffer.contents buf in
          Buffer.clear buf;
          ract s;
         );
        None
    | n ->
        let rec eat ppos =
          let nlpos =
            match Bytes.index_from scratch ppos '\n' with
            | pos -> if pos >= n then -1 else pos
            | (exception Not_found) -> -1
          in
          if nlpos >= 0
          then (
            Buffer.add_subbytes buf scratch ppos (nlpos-ppos);
            let s = Buffer.contents buf in
            Buffer.clear buf;
            ract s;
            eat (nlpos+1);
           )
          else (
            Buffer.add_subbytes buf scratch ppos (n-ppos);
            Some fd
           )
        in eat 0
;;

let remoteopen path =
  try Some (Unix.openfile path [Unix.O_NONBLOCK; Unix.O_RDONLY] 0o0)
  with exn ->
    adderrfmt "remoteopen" "error opening %S: %s" path @@ exntos exn;
    None
;;

let () =
  let gcconfig = ref E.s in
  let trimcachepath = ref E.s in
  let rcmdpath = ref E.s in
  let pageno = ref None in
  let rootwid = ref 0 in
  let openlast = ref false in
  let nofc = ref false in
  let doreap = ref false in
  selfexec := Sys.executable_name;
  Arg.parse
    (Arg.align
        [("-p", Arg.String (fun s -> state.password <- s),
         "<password> Set password");

         ("-f", Arg.String
           (fun s ->
             Config.fontpath := s;
             selfexec := !selfexec ^ " -f " ^ Filename.quote s;
           ),
         "<path> Set path to the user interface font");

         ("-c", Arg.String
           (fun s ->
             selfexec := !selfexec ^ " -c " ^ Filename.quote s;
             Config.confpath := s),
         "<path> Set path to the configuration file");

         ("-last", Arg.Set openlast, " Open last document");

         ("-page", Arg.Int (fun pageno1 -> pageno := Some (pageno1-1)),
         "<page-number> Jump to page");

         ("-tcf", Arg.String (fun s -> trimcachepath := s),
         "<path> Set path to the trim cache file");

         ("-dest", Arg.String (fun s -> state.nameddest <- s),
         "<named-destination> Set named destination");

         ("-wtmode", Arg.Set wtmode, " Operate in wt mode");
         ("-cxack", Arg.Set cxack, " Cut corners");

         ("-remote", Arg.String (fun s -> rcmdpath := s),
         "<path> Set path to the remote commands source");

         ("-origin", Arg.String (fun s -> state.origin <- s),
         "<original-path> Set original path");

         ("-gc", Arg.Set_string gcconfig,
          "<script-path> Collect garbage with the help of a script");

         ("-nofc", Arg.Set nofc, " Do not use fontconfig");

         ("-v", Arg.Unit (fun () ->
           Printf.printf
             "%s\nconfiguration path: %s\n"
             (version ())
             Config.defconfpath
           ;
           exit 0), " Print version and exit");

         ("-embed", Arg.Set_int rootwid,
          "<window-id> Embed into window")
        ]
    )
    (fun s -> state.path <- s)
    ("Usage: " ^ Sys.argv.(0) ^ " [options] some.pdf\nOptions:")
  ;
  if !wtmode
  then selfexec := !selfexec ^ " -wtmode";

  let histmode = emptystr state.path && not !openlast in

  if not (Config.load !openlast)
  then dolog "failed to load configuration";

  begin match !pageno with
  | Some pageno -> state.anchor <- (pageno, 0.0, 0.0)
  | None -> ()
  end;

  if nonemptystr !gcconfig
  then (
    let (c, s) =
      match Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 with
      | (exception exn) -> error "socketpair for gc failed: %s" @@ exntos exn
      | fds -> fds
    in
    match spawn !gcconfig [(c, 0); (c, 1); (s, -1)] with
    | (exception exn) -> error "failed to execute gc script: %s" @@ exntos exn
    | _pid ->
        Ne.clo c @@ (fun s -> error "failed to close gc fd %s" s);
        Config.gc s;
        exit 0
   );

  let wsfd, winw, winh = Wsi.init (object (self)
    val mutable m_clicks = 0
    val mutable m_click_x = 0
    val mutable m_click_y = 0
    val mutable m_lastclicktime = infinity

    method private cleanup =
      state.roam <- noroam;
      Hashtbl.iter (fun _ opaque -> clearmark opaque) state.pagemap
    method expose = G.postRedisplay "expose"
    method visible v =
      let name =
        match v with
        | Wsi.Unobscured -> "unobscured"
        | Wsi.PartiallyObscured -> "partiallyobscured"
        | Wsi.FullyObscured -> "fullyobscured"
      in
      vlog "visibility change %s" name
    method display = display ()
    method map mapped = vlog "mapped %b" mapped
    method reshape w h =
      self#cleanup;
      reshape w h
    method mouse b d x y m =
      if d && canselect ()
      then (
        (* http://blogs.msdn.com/b/oldnewthing/archive/2004/10/18/243925.aspx *)
        m_click_x <- x;
        m_click_y <- y;
        if b = 1
        then (
          let t = now () in
          if abs x - m_click_x > 10
            || abs y - m_click_y > 10
            || abs_float (t -. m_lastclicktime) > 0.3
          then m_clicks <- 0;
          m_clicks <- m_clicks + 1;
          m_lastclicktime <- t;
          if m_clicks = 1
          then (
            self#cleanup;
            G.postRedisplay "cleanup";
            state.uioh <- state.uioh#button b d x y m;
          )
          else state.uioh <- state.uioh#multiclick m_clicks x y m
        )
        else (
          self#cleanup;
          m_clicks <- 0;
          m_lastclicktime <- infinity;
          state.uioh <- state.uioh#button b d x y m
        );
      )
      else (
        state.uioh <- state.uioh#button b d x y m
      )
    method motion x y =
      state.mpos <- (x, y);
      state.uioh <- state.uioh#motion x y
    method pmotion x y =
      state.mpos <- (x, y);
      state.uioh <- state.uioh#pmotion x y
    method key k m =
      let mascm = m land (
        Wsi.altmask + Wsi.shiftmask + Wsi.ctrlmask + Wsi.metamask
      ) in
      let keyboard k m =
        let x = state.x and y = state.y in
        keyboard k m;
        if x != state.x || y != state.y then self#cleanup
      in
      match state.keystate with
      | KSnone ->
          let km = k, mascm in
          begin
            match
              let modehash = state.uioh#modehash in
              try Hashtbl.find modehash km
              with Not_found ->
                try Hashtbl.find (findkeyhash conf "global") km
                with Not_found -> KMinsrt (k, m)
            with
            | KMinsrt (k, m) -> keyboard k m
            | KMinsrl l -> List.iter (fun (k, m) -> keyboard k m) l
            | KMmulti (l, r) -> state.keystate <- KSinto (l, r)
          end
      | KSinto ((k', m') :: [], insrt) when k'=k && m' land mascm = m' ->
          List.iter (fun (k, m) -> keyboard k m) insrt;
          state.keystate <- KSnone
      | KSinto ((k', m') :: keys, insrt) when k'=k && m' land mascm = m' ->
          state.keystate <- KSinto (keys, insrt)
      | KSinto _ -> state.keystate <- KSnone

    method enter x y =
      state.mpos <- (x, y);
      state.uioh <- state.uioh#pmotion x y
    method leave = state.mpos <- (-1, -1)
    method winstate wsl = state.winstate <- wsl
    method quit = raise Quit
  end) !rootwid conf.cwinw conf.cwinh platform in

  setbgcol conf.bgcolor;
  state.wsfd <- wsfd;

  if not (
    List.exists GlMisc.check_extension
      [ "GL_ARB_texture_rectangle"
      ; "GL_EXT_texture_recangle"
      ; "GL_NV_texture_rectangle" ]
  )
  then (dolog "OpenGL does not suppport rectangular textures"; exit 1);

  if (
    let r = GlMisc.get_string `renderer in
    let p = "Mesa DRI Intel(" in
    let l = String.length p in
    String.length r > l && String.sub r 0 l = p
  )
  then (
    defconf.sliceheight <- 1024;
    defconf.texcount <- 32;
    defconf.usepbo <- true;
  );

  let cs, ss =
    match Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 with
    | (exception exn) ->
        dolog "socketpair failed: %s" @@ exntos exn;
        exit 1
    | (r, w) ->
        cloexec r;
        cloexec w;
        r, w
  in

  setcheckers conf.checkers;

  opengl_has_pbo := GlMisc.check_extension "GL_ARB_pixel_buffer_object";

  init cs (
    conf.angle, conf.fitmodel, (conf.trimmargins, conf.trimfuzz),
    conf.texcount, conf.sliceheight, conf.mustoresize, conf.colorspace,
    !Config.fontpath, !trimcachepath,
    !opengl_has_pbo,
    not !nofc
  );
  List.iter GlArray.enable [`texture_coord; `vertex];
  state.ss <- ss;
  reshape ~firsttime:true winw winh;
  state.uioh <- uioh;
  if histmode
  then (
    Wsi.settitle "llpp (history)";
    enterhistmode ();
  )
  else (
    state.text <- "Opening " ^ (mbtoutf8 state.path);
    opendoc state.path state.password;
  );
  display ();
  Wsi.mapwin ();
  Wsi.setcursor Wsi.CURSOR_INHERIT;
  Sys.set_signal Sys.sighup (Sys.Signal_handle (fun _ -> reload ()));

  let rec reap () =
    match Unix.waitpid [Unix.WNOHANG] ~-1 with
    | (exception (Unix.Unix_error (Unix.ECHILD, _, _))) -> ()
    | (exception exn) -> dolog "Unix.waitpid: %s" @@ exntos exn
    | 0, _ -> ()
    | _pid, _status -> reap ()
  in
  Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> doreap := true));

  let optrfd =
    ref (
      if nonemptystr !rcmdpath
      then remoteopen !rcmdpath
      else None
    )
  in

  let rec loop deadline =
    if !doreap
    then (
      doreap := false;
      reap ()
    );
    let r = [state.ss; state.wsfd] in
    let r =
      match !optrfd with
      | None -> r
      | Some fd -> fd :: r
    in
    if state.redisplay
    then (
      state.redisplay <- false;
      display ();
    );
    let timeout =
      let now = now () in
      if deadline > now
      then (
        if deadline = infinity
        then ~-.1.0
        else max 0.0 (deadline -. now)
      )
      else 0.0
    in
    let r, _, _ =
      try Unix.select r [] [] timeout
      with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], []
    in
    begin match r with
    | [] ->
        state.ghyll None;
        let newdeadline =
          if state.ghyll == noghyll
          then
            match state.autoscroll with
            | Some step when step != 0 ->
                let y = state.y +. float step in
                let fy = float @@ if conf.maxhfit then state.winh else 0 in
                let y =
                  if y < 0.0
                  then state.maxy -. fy
                  else if y >= state.maxy -. fy then 0. else y
                in
                if state.mode = View
                then gotoxy_and_clear_text state.x y
                else gotoxy state.x y;
                deadline +. 0.01
            | _ -> infinity
          else deadline +. 0.01
        in
        loop newdeadline

    | l ->
        let rec checkfds = function
          | [] -> ()
          | fd :: rest when fd = state.ss ->
              let cmd = rcmd state.ss in
              act cmd;
              checkfds rest

          | fd :: rest when fd = state.wsfd ->
              Wsi.readresp fd;
              checkfds rest

          | fd :: rest when Some fd = !optrfd ->
              begin match remote fd with
              | None -> optrfd := remoteopen !rcmdpath;
              | opt -> optrfd := opt
              end;
              checkfds rest

          | _ :: rest ->
              dolog "select returned unknown descriptor";
              checkfds rest
        in
        checkfds l;
        let newdeadline =
          let deadline1 =
            if deadline = infinity
            then now () +. 0.01
            else deadline
          in
          match state.autoscroll with
          | Some step when step != 0 -> deadline1
          | _ -> if state.ghyll == noghyll then infinity else deadline1
        in
        loop newdeadline
    end;
  in
  try
    loop infinity;
  with Quit ->
    Config.save leavebirdseye;
    if hasunsavedchanges ()
    then save ();
;;
