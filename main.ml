open Utils;;
open Config;;
open Glutils;;
open Listview;;

let selfexec = ref E.s;;
let ignoredoctitlte = ref false;;
let layouth = ref ~-1;;
let checkerstexid = ref None;;

let debugrect (x0, y0, x1, y1, x2, y2, x3, y3) =
  dolog {|rect {
         x0,y0=(% f, % f)
         x1,y1=(% f, % f)
         x2,y2=(% f, % f)
         x3,y3=(% f, % f)
         }|} x0 y0 x1 y1 x2 y2 x3 y3;
;;

let pgscale h = truncate (float h *. conf.pgscale);;

let hscrollh () =
  if ((conf.scrollb land scrollbhv != 0) && (state.w > state.winw))
     || state.uioh#alwaysscrolly
  then conf.scrollbw
  else 0
;;

let setfontsize n =
  fstate.fontsize <- n;
  fstate.wwidth <- measurestr fstate.fontsize "w";
  fstate.maxrows <- (state.winh - fstate.fontsize - 1) / (fstate.fontsize + 1);
;;

let launchpath () =
  if emptystr conf.pathlauncher
  then dolog "%s" state.path
  else (
    let command =
      Str.global_replace Utils.Re.percent state.path conf.pathlauncher in
    match spawn command [] with
    | _pid -> ()
    | exception exn -> dolog "failed to execute `%s': %s" command @@ exntos exn
  );
;;

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
       | _ -> f rest
       end
    | [] -> d
  in
  f state.layout
;;

let getunder x y =
  let g opaque l px py =
    if state.bzoom
    then (
      match Ffi.rectofblock opaque px py with
      | Some [|x0;x1;y0;y1|] ->
         let rect = (x0, y0, x1, y0, x1, y1, x0, y1) in
         let color = (0.0, 0.0, 1.0 /. (l.pageno mod 3 |> float), 0.5) in
         state.rects <- [l.pageno, color, rect];
         postRedisplay "getunder";
      | _ -> ()
    );
    let under = Ffi.whatsunder opaque px py in
    if under = Unone then None else Some under
  in
  onppundermouse g x y Unone
;;

let unproject x y =
  let g opaque l x y =
    match Ffi.unproject opaque x y with
    | Some (x, y) -> Some (Some (opaque, l.pageno, x, y))
    | None -> None
  in
  onppundermouse g x y None;
;;

let showtext c s =
  state.text <- Printf.sprintf "%c%s" c s;
  postRedisplay "showtext";
;;

let impmsg fmt = Format.ksprintf (fun s -> showtext '!' s) fmt;;

let pipesel opaque cmd =
  if Ffi.hassel opaque
  then pipef ~closew:false "pipesel"
         (fun w ->
           Ffi.copysel w opaque;
           postRedisplay "pipesel"
         ) cmd
;;

let paxunder x y =
  let g opaque l px py =
    if Ffi.markunder opaque px py conf.paxmark
    then
      Some (fun () ->
          match getopaque l.pageno with
          | None -> ()
          | Some opaque -> pipesel opaque conf.paxcmd
        )
    else None
  in
  postRedisplay "paxunder";
  if conf.paxmark = Mark_page
  then
    List.iter (fun l ->
        match getopaque l.pageno with
        | None -> ()
        | Some opaque -> Ffi.clearmark opaque) state.layout;
  state.roam <- onppundermouse g x y (fun () -> impmsg "whoopsie daisy");
;;

let undertext = function
  | Unone -> "none"
  | Ulinkuri s -> s
  | Utext s -> "font: " ^ s
  | Uannotation (opaque, slinkindex) ->
     "annotation: " ^ Ffi.getannotcontents opaque slinkindex
;;

let updateunder x y =
  match getunder x y with
  | Unone -> Wsi.setcursor Wsi.CURSOR_INHERIT
  | Ulinkuri uri ->
     if conf.underinfo then showtext 'u' ("ri: " ^ uri);
     Wsi.setcursor Wsi.CURSOR_INFO
  | Utext s ->
     if conf.underinfo then showtext 'f' ("ont: " ^ s);
     Wsi.setcursor Wsi.CURSOR_TEXT
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
    match [@warning "-4"] key with
    | Keys.Ascii ('0'..'9' as c) -> addchar text c
    | Keys.Ascii ('k' | 'm' | 'g' | 'K' | 'M' | 'G' as c) ->
       addchar text @@ asciilower c
    | _ ->
       state.text <- "invalid key";
       text
  in
  TEcont text
;;

let wcmd fmt =
  let b = Buffer.create 16 in
  Printf.kbprintf
    (fun b ->
      let b = Buffer.to_bytes b in
      Ffi.wcmd state.ss b @@ Bytes.length b
    ) b fmt
;;

let nogeomcmds = function
  | s, [] -> emptystr s
  | _ -> false
;;

let layoutN ((columns, coverA, coverB), b) x y sw sh =
  let rec fold accu n =
    if n = Array.length b
    then accu
    else
      let pdimno, dx, vy, (_, w, h, xoff) = b.(n) in
      if (vy - y) > sh &&
           (n = coverA - 1
            || n = state.pagecount - coverB
            || (n - coverA) mod columns = columns - 1)
      then accu
      else
        let accu =
          if vy + h > y
          then
            let pagey = max 0 (y - vy) in
            let pagedispy = if pagey > 0 then 0 else vy - y in
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
              } :: accu
            else accu
          else accu
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
              } :: accu
            else accu
          else accu
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
    | Csplit s -> layoutS s x y sw sh
  else []
;;

let maxy () = state.maxy - if conf.maxhfit then state.winh else 0;;
let clamp incr = bound (state.y + incr) 0 @@ maxy ();;

let itertiles l f =
  let tilex = l.pagex mod conf.tilew in
  let tiley = l.pagey mod conf.tileh in

  let col = l.pagex / conf.tilew in
  let row = l.pagey / conf.tileh in

  let rec rowloop row y0 dispy h =
    if h != 0
    then
      let dh = conf.tileh - y0 in
      let dh = min h dh in
      let rec colloop col x0 dispx w =
        if w != 0
        then
          let dw = conf.tilew - x0 in
          let dw = min w dw in
          f col row dispx dispy x0 y0 dw dh;
          colloop (col+1) 0 (dispx+dw) (w-dw)
      in
      colloop col tilex l.pagedispx l.pagevw;
      rowloop (row+1) 0 (dispy+dh) (h-dh)
  in
  if l.pagevw > 0 && l.pagevh > 0
  then rowloop row tiley l.pagedispy l.pagevh;
;;

let gettileopaque l col row =
  let key = l.pageno, state.gen, conf.colorspace,
            conf.angle, l.pagew, l.pageh, col, row in
  try Some (Hashtbl.find state.tilemap key)
  with Not_found -> None
;;

let puttileopaque l col row gen colorspace angle opaque size elapsed =
  let key = l.pageno, gen, colorspace, angle, l.pagew, l.pageh, col, row in
  Hashtbl.add state.tilemap key (opaque, size, elapsed)
;;

let drawtiles l color =
  GlDraw.color color;
  Ffi.begintiles ();
  let f col row x y tilex tiley w h =
    match gettileopaque l col row with
    | Some (opaque, _, t) ->
       let params = x, y, w, h, tilex, tiley in
       if conf.invert
       then GlTex.env (`mode `blend);
       Ffi.drawtile params opaque;
       if conf.invert
       then GlTex.env (`mode `modulate);
       if conf.debug
       then (
         Ffi.endtiles ();
         let s = Printf.sprintf "%d[%d,%d] %f sec" l.pageno col row t in
         let w = measurestr fstate.fontsize s in
         GlDraw.color (0.0, 0.0, 0.0);
         filledrect
           (float (x-2))
           (float (y-2))
           (float (x+2) +. w)
           (float (y + fstate.fontsize + 2));
         GlDraw.color color;
         drawstring fstate.fontsize x (y + fstate.fontsize - 1) s;
         Ffi.begintiles ();
       );

    | None ->
       Ffi.endtiles ();
       let w = let lw = state.winw - x in min lw w
       and h = let lh = state.winh - y in min lh h
       in
       if conf.invert
       then GlTex.env (`mode `blend);
       begin match !checkerstexid with
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
          Raw.sets_float Glutils.vraw ~pos:0
            [| x0; y0; x0; y1; x1; y0; x1; y1 |];
          Raw.sets_float Glutils.traw ~pos:0
            [| tx0; ty0; tx0; ty1; tx1; ty0; tx1; ty1 |];
          GlArray.vertex `two Glutils.vraw;
          GlArray.tex_coord `two Glutils.traw;
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
       Ffi.begintiles ();
  in
  itertiles l f;
  Ffi.endtiles ();
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
        | Csplit _ | Csingle _ | Cmulti _ -> false
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
                  then Ffi.getpbo w h conf.colorspace
                  else ~< "0"
                in
                wcmd "tile %s %d %d %d %d %s" (~> p) x y w h (~> pbo);
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
  layout x y w h;
;;

let load pages =
  let rec loop pages =
    if state.currently = Idle
    then
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
  then load (preloadlayout state.x state.y state.winw state.winh);
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
  let y = bound y 0 state.maxy in
  let y, layout =
    let layout = layout x y state.winw state.winh in
    postRedisplay "gotoxy ready";
    y, layout
  in
  state.x <- x;
  state.y <- y;
  state.layout <- layout;
  begin match state.mode with
  | LinkNav ln ->
     begin match ln with
     | Ltexact (pageno, linkno) ->
        let rec loop = function
          | [] ->
             state.lnava <- Some (pageno, linkno);
             state.mode <- LinkNav (Ltgendir 0)
          | l :: _ when l.pageno = pageno ->
             begin match getopaque pageno with
             | None -> state.mode <- LinkNav (Ltnotready (pageno, 0))
             | Some opaque ->
                let x0, y0, x1, y1 = Ffi.getlinkrect opaque linkno in
                if not (x0 >= l.pagex && x1 <= l.pagex + l.pagevw
                        && y0 >= l.pagey && y1 <= l.pagey + l.pagevh)
                then state.mode <- LinkNav (Ltgendir 0)
             end
          | _ :: rest -> loop rest
        in
        loop layout
     | Ltnotready _ | Ltgendir _ -> ()
     end
  | Birdseye _ | Textentry _ | View -> ()
  end;
  begin match state.mode with
  | Birdseye (conf, leftx, pageno, hooverpageno, anchor) ->
     if not (pagevisible layout pageno)
     then (
       match state.layout with
       | [] -> ()
       | l :: _ ->
          state.mode <- Birdseye (conf, leftx, l.pageno, hooverpageno, anchor)
     )
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
                      else if dir > 0 then LDfirst else LDlast
                    in
                    Ffi.findlink opaque ld
                  in
                  match link with
                  | Lnotfound -> loop rest
                  | Lfound n ->
                     showlinktype (Ffi.getlink opaque n);
                     Ltexact (l.pageno, n)
          in
          loop state.layout
        in
        state.mode <- LinkNav linknav
     | Ltexact _ -> ()
     end
  | Textentry _ | View -> ()
  end;
  preload layout;
  if conf.updatecurs
  then (
    let mx, my = state.mpos in
    updateunder mx my;
  );
;;

let conttiling pageno opaque =
  tilepage pageno opaque
    (if conf.preload
     then preloadlayout state.x state.y state.winw state.winh
     else state.layout)
;;

let gotoxy x y =
  if not conf.verbose then state.text <- E.s;
  gotoxy x y;
;;

let getanchory (n, top, dtop) =
  let y, h = getpageyh n in
  if conf.presentation
  then
    let ips = calcips h in
    y + truncate (top*.float h -. dtop*.float ips) + ips;
  else y + truncate (top*.float h -. dtop*.float conf.interpagespace)
;;

let addnav () =
  state.nav <-
    { past = getanchor () :: state.nav.past
    ; future = []
    }
;;

let gotopage n top =
  let y, h = getpageyh n in
  let y = y + (truncate (top *. float h)) in
  gotoxy state.x y
;;

let gotopage1 n top =
  let y = getpagey n in
  let y = y + top in
  gotoxy state.x y
;;

let invalidate s f =
  Glutils.redisplay := false;
  state.layout <- [];
  state.pdims <- [];
  state.rects <- [];
  state.rects1 <- [];
  match state.geomcmds with
  | ps, [] when emptystr ps ->
     f ();
     state.geomcmds <- s, [];
  | ps, [] -> state.geomcmds <- ps, [s, f];
  | ps, (s', _) :: rest when s' = s -> state.geomcmds <- ps, ((s, f) :: rest);
  | ps, cmds -> state.geomcmds <- ps, ((s, f) :: cmds);
;;

let flushpages () =
  Hashtbl.iter (fun _ opaque -> wcmd "freepage %s" (~> opaque)) state.pagemap;
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

let fillhelp () =
  state.help <-
    let sl = keystostrlist conf in
    let rec loop accu =
      function | [] -> accu
               | s :: rest -> loop ((s, 0, Noaction) :: accu) rest
    in Help.makehelp conf.urilauncher
       @ (("", 0, Noaction) :: loop [] sl) |> Array.of_list
;;

let opendoc path password =
  state.path <- path;
  state.password <- password;
  state.gen <- state.gen + 1;
  state.docinfo <- [];
  state.outlines <- [||];

  flushpages ();
  Ffi.setaalevel conf.aalevel;
  Ffi.setpapercolor conf.papercolor;
  let titlepath =
    if emptystr state.origin
    then path
    else state.origin
  in
  Wsi.settitle ("llpp " ^ mbtoutf8 (Filename.basename titlepath));
  wcmd "open %d %d %s\000%s\000%s\000"
    (btod conf.usedoccss) !layouth
    path password conf.css;
  invalidate "reqlayout"
    (fun () ->
      wcmd "reqlayout %d %d %d %s\000"
        conf.angle (FMTE.to_int conf.fitmodel)
        (stateh state.winh) state.nameddest
    );
  fillhelp ();
;;

let reload () =
  state.anchor <- getanchor ();
  state.reload <- Some (state.x, state.y, now ());
  opendoc state.path state.password;
;;

let scalecolor c = let c = c *. conf.colorscale in (c, c, c);;
let scalecolor2 (r, g, b) =
  (r *. conf.colorscale, g *. conf.colorscale, b *. conf.colorscale);
;;

let docolumns columns =
  match columns with
  | Csingle _ ->
     let a = Array.make state.pagecount (-1, -1, -1, (-1, -1, -1, -1)) in
     let rec loop pageno pdimno pdim y ph pdims =
       if pageno != state.pagecount
       then
         let pdimno, ((_, w, h, xoff) as pdim), pdims =
           match pdims with
           | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
              pdimno+1, pdim, rest
           | _ ->
              pdimno, pdim, pdims
         in
         let x = max 0 (((state.winw - w) / 2) - xoff) in
         let y =
           y + (if conf.presentation
                then (if pageno = 0 then calcips h else calcips ph + calcips h)
                else (if pageno = 0 then 0 else conf.interpagespace))
         in
         a.(pageno) <- (pdimno, x, y, pdim);
         loop (pageno+1) pdimno pdim (y + h) h pdims
     in
     loop 0 ~-1 (-1,-1,-1,-1) 0 0 state.pdims;
     conf.columns <- Csingle a;

  | Cmulti ((columns, coverA, coverB), _) ->
     let a = Array.make state.pagecount (-1, -1, -1, (-1, -1, -1, -1)) in
     let rec loop pageno pdimno pdim x y rowh pdims =
       let rec fixrow m =
         if m = pageno then () else
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
           | _ -> pdimno, pdim, pdims
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
       if pageno != state.pagecount
       then
         let pdimno, ((_, w, h, _) as pdim), pdims =
           match pdims with
           | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
              pdimno+1, pdim, rest
           | _ -> pdimno, pdim, pdims
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
       gotoxy state.x (max 0 (y - top))
    | Textentry _ | View | LinkNav _ ->
       let y = getanchory state.anchor in
       let y = min y (state.maxy - state.winh) in
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
    else float state.x /. float state.w
  in
  invalidate "geometry"
    (fun () ->
      state.w <- w;
      if not firsttime
      then state.x <- truncate (relx *. float w);
      let w =
        match conf.columns with
        | Csingle _ -> w
        | Cmulti ((c, _, _), _) -> (w - (c-1)*conf.interpagespace) / c
        | Csplit (c, _) -> w * c
      in
      wcmd "geometry %d %d %d" w (stateh h) (FMTE.to_int conf.fitmodel)
    );
;;

let gctiles () =
  let len = Queue.length state.tilelru in
  let layout = lazy (if conf.preload
                     then preloadlayout state.x state.y state.winw state.winh
                     else state.layout) in
  let rec loop qpos =
    if state.memused > conf.memlimit
    then (
      if qpos < len
      then
        let (k, p, s) as lruitem = Queue.pop state.tilelru in
        let n, gen, colorspace, angle, pagew, pageh, col, row = k in
        let (_, pw, ph, _) = getpagedim n in
        if   gen = state.gen
             && colorspace = conf.colorspace
             && angle = conf.angle
             && pagew = pw
             && pageh = ph
             && (
               let x = col*conf.tilew and y = row*conf.tileh in
               tilevisible (Lazy.force_val layout) n x y
             )
        then Queue.push lruitem state.tilelru
        else (
          Ffi.freepbo p;
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

let gotopagexy1 pageno x y =
  let _,w1,h1,leftx = getpagedim pageno in
  let top = y /. (float h1) in
  let left = x /. (float w1) in
  let py, w, h = getpageywh pageno in
  let wh = state.winh in
  let x = left *. (float w) in
  let x = leftx + state.x + truncate x in
  let sx =
    if x < 0 || x >= state.winw
    then state.x - x
    else state.x
  in
  let pdy = truncate (top *. float h) in
  let y' = py + pdy in
  let dy = y' - state.y in
  let sy =
    if x != state.x || not (dy > 0 && dy < wh)
    then (
      if conf.presentation
      then
        if abs (py - y') > wh
        then y'
        else py
      else y';
    )
    else state.y
  in
  if state.x != sx || state.y != sy
  then gotoxy sx sy
  else gotoxy state.x state.y;
;;

let gotopagexy pageno x y =
  match state.mode with
  | Birdseye _ -> gotopage pageno 0.0
  | Textentry _ | View | LinkNav _ -> gotopagexy1 pageno x y
;;

let getpassword () =
  let passcmd = getenvdef "LLPP_ASKPASS" conf.passcmd in
  if emptystr passcmd
  then E.s
  else getcmdoutput (fun s ->
           impmsg "error getting password: %s" s;
           dolog "%s" s) passcmd;
;;

let pgoto opaque pageno x y =
  let pdimno = getpdimno pageno in
  let x, y = Ffi.project opaque pageno pdimno x y in
  gotopagexy pageno x y;
;;

let act cmds =
  (* dolog "%S" cmds; *)
  let spl = splitatchar cmds ' ' in
  let scan s fmt f =
    try Scanf.sscanf s fmt f
    with exn ->
      dolog "error processing '%S': %s" cmds @@ exntos exn;
      exit 1
  in
  let addoutline outline =
    match state.currently with
    | Outlining outlines -> state.currently <- Outlining (outline :: outlines)
    | Idle -> state.currently <- Outlining [outline]
    | Loading _ | Tiling _ ->
       dolog "invalid outlining state";
       logcurrently state.currently
  in
  match spl with
  | "clear", "" ->
     state.pdims <- [];
     state.uioh#infochanged Pdim;

  | "clearrects", "" ->
     state.rects <- state.rects1;
     postRedisplay "clearrects";

  | "continue", args ->
     let n = scan args "%u" (fun n -> n) in
     state.pagecount <- n;
     begin match state.currently with
     | Outlining l ->
        state.currently <- Idle;
        state.outlines <- Array.of_list (List.rev l)
     | Idle | Loading _ | Tiling _ -> ()
     end;

     let cur, cmds = state.geomcmds in
     if emptystr cur then error "empty geomcmd";

     begin match List.rev cmds with
     | [] ->
        state.geomcmds <- E.s, [];
        represent ();
     | (s, f) :: rest ->
        f ();
        state.geomcmds <- s, List.rev rest;
     end;
     postRedisplay "continue";

  | "msg", args ->
     showtext ' ' args

  | "vmsg", args ->
     if conf.verbose then showtext ' ' args

  | "emsg", args ->
     Buffer.add_string state.errmsgs args;
     Buffer.add_char state.errmsgs '\n';
     if not state.newerrmsgs
     then (
       state.newerrmsgs <- true;
       postRedisplay "error message";
     )

  | "progress", args ->
     let progress, text =
       scan args "%f %n"
         (fun f pos -> f, String.sub args pos (String.length args - pos))
     in
     state.text <- text;
     state.progress <- progress;
     postRedisplay "progress"

  | "firstmatch", args ->
     let pageno, c, x0, y0, x1, y1, x2, y2, x3, y3 =
       scan args "%u %d %f %f %f %f %f %f %f %f"
         (fun p c x0 y0 x1 y1 x2 y2 x3 y3 ->
           (p, c, x0, y0, x1, y1, x2, y2, x3, y3))
     in
     let y = (getpagey pageno) + truncate y0 in
     let x =
       if (state.x < - truncate x0) || (state.x > state.winw - truncate x1)
       then state.winw/2 - truncate (x0 /. 2. +. x1 /. 2.)
       else state.x
     in
     addnav ();
     gotoxy x y;
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
        let preloadedpages =
          if conf.preload
          then preloadlayout state.x state.y state.winw state.winh
          else state.layout
        in
        let evict () =
          let set = List.fold_left (fun s l -> IntSet.add l.pageno s)
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
                     else if dir > 0 then LDfirst else LDlast
                   in
                   Ffi.findlink pageopaque ld
                 in
                 match link with
                 | Lnotfound -> ()
                 | Lfound n ->
                    showlinktype (Ffi.getlink pageopaque n);
                    state.mode <- LinkNav (Ltexact (l.pageno, n))
               )
            | LinkNav (Ltgendir _)
            | LinkNav (Ltexact _)
            | View
            | Birdseye _
            | Textentry _ -> ()
          );

          if visible && layoutready state.layout
          then postRedisplay "page";
        )

     | Idle | Tiling _ | Outlining _ ->
        dolog "Inconsistent loading state";
        logcurrently state.currently;
        exit 1
     end

  | "tile" , args ->
     let (x, y, opaques, size, t) =
       scan args "%u %u %s %u %f" (fun x y p size t -> (x, y, p, size, t))
     in
     let opaque = ~< opaques in
     begin match state.currently with
     | Tiling (l, pageopaque, cs, angle, gen, col, row, tilew, tileh) ->
        vlog "tile %d [%d,%d] took %f sec" l.pageno col row t;

        Ffi.unmappbo opaque;
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

          state.currently <- Idle;
          if    gen = state.gen
                && conf.colorspace = cs
                && conf.angle = angle
                && tilevisible state.layout l.pageno x y
          then conttiling l.pageno pageopaque;

          preload state.layout;
          if   gen = state.gen
               && conf.colorspace = cs
               && conf.angle = angle
               && tilevisible state.layout l.pageno x y
               && layoutready state.layout
          then postRedisplay "tile nothrottle";
        )

     | Idle | Loading _ | Outlining _ ->
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
     state.pdims <- pdim :: state.pdims;
     state.uioh#infochanged Pdim

  | "o", args ->
     let (l, n, t, h, pos) =
       scan args "%u %u %d %u %n" (fun l n t h pos -> l, n, t, h, pos)
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
     let (n, l, t) = scan args "%u %d %d" (fun n l t -> n, l, t) in
     state.reprf <- (fun () -> gotopagexy n (float l) (float t))

  | "info", args ->
     let c, v = splitatchar args '\t' in
     let s =
       if nonemptystr v
       then
         if c = "Title"
         then (
           conf.title <- v;
           if not !ignoredoctitlte then Wsi.settitle v;
           args
         )
         else
           if let len = String.length c in
              len > 6 && ((String.sub c (len-4) 4) = "date")
           then (
             if String.length v >= 7 && v.[0] = 'D' && v.[1] = ':'
             then
               let b = Buffer.create 10 in
               Printf.bprintf b "%s\t" c;
               let sub p l c =
                 try
                   Buffer.add_substring b v p l;
                   Buffer.add_char b c;
                 with exn -> Buffer.add_string b @@ exntos exn
               in
               sub 2 4 '/';
               sub 6 2 '/';
               sub 8 2 ' ';
               sub 10 2 ':';
               sub 12 2 ':';
               sub 14 2 ' ';
               Printf.bprintf b  "[%s]" v;
               Buffer.contents b
             else args
           )
           else args
       else args
     in
     state.docinfo <- (1, s) :: state.docinfo

  | "infoend", "" ->
     state.docinfo <- List.rev state.docinfo;
     state.uioh#infochanged Docinfo

  | "pass", args ->
     if args = "fail"
     then Wsi.settitle "Wrong password";
     let password = getpassword () in
     if emptystr password
     then error "document is password protected"
     else opendoc state.path password

  | _ -> error "unknown cmd `%S'" cmds
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
  | Csingle _ | Cmulti _ ->
     if nonemptystr pattern
     then
       let pn, py =
         match state.layout with
         | [] -> 0, 0
         | l :: _ -> l.pageno, (l.pagey + if forward then 0 else 0*l.pagevh)
       in
       wcmd "search %d %d %d %d,%s\000"
         (btod conf.icase) pn py (btod forward) pattern;
;;

let intentry text key =
  let text =
    if emptystr text && key = Keys.Ascii '-'
    then addchar text '-'
    else
      match [@warning "-4"] key with
      | Keys.Ascii ('0'..'9' as c) -> addchar text c
      | _ ->
         state.text <- "invalid key";
         text
  in
  TEcont text
;;

let linknact f s =
  if nonemptystr s
  then
    let n =
      let l = String.length s in
      let rec loop pos n =
        if pos = l
        then n
        else
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
            let m = Ffi.getlinkcount opaque in
            if n < m
            then
              let under = Ffi.getlink opaque n in
              f under
            else loop (n-m) rest
    in
    loop n state.layout;
;;

let linknentry text key = match [@warning "-4"] key with
  | Keys.Ascii ('a' .. 'z' as c)  ->
     let text = addchar text c in
     linknact (fun under -> state.text <- undertext under) text;
     TEcont text
  | _ ->
     state.text <- Printf.sprintf "invalid key %s" @@ Keys.to_string key;
     TEcont text
;;

let textentry text key = match [@warning "-4"] key with
  | Keys.Ascii c -> TEcont (addchar text c)
  | Keys.Code c -> TEcont (text ^ toutf8 c)
  | _ -> TEcont text
;;

let reqlayout angle fitmodel =
  if nogeomcmds state.geomcmds
  then state.anchor <- getanchor ();
  conf.angle <- angle mod 360;
  if conf.angle != 0
  then (
    match state.mode with
    | LinkNav _ -> state.mode <- View
    | Birdseye _ | Textentry _ | View -> ()
  );
  conf.fitmodel <- fitmodel;
  invalidate "reqlayout"
    (fun () -> wcmd "reqlayout %d %d %d"
                 conf.angle (FMTE.to_int conf.fitmodel) (stateh state.winh));
;;

let settrim trimmargins trimfuzz =
  if nogeomcmds state.geomcmds
  then state.anchor <- getanchor ();
  conf.trimmargins <- trimmargins;
  conf.trimfuzz <- trimfuzz;
  let x0, y0, x1, y1 = trimfuzz in
  invalidate "settrim"
    (fun () -> wcmd "settrim %d %d %d %d %d"
                 (btod conf.trimmargins) x0 y0 x1 y1);
  flushpages ();
;;

let setzoom zoom =
  let zoom = max 0.0001 zoom in
  if zoom <> conf.zoom
  then (
    state.prevzoom <- (conf.zoom, state.x);
    conf.zoom <- zoom;
    reshape state.winw state.winh;
    state.text <- Printf.sprintf "zoom is now %-5.2f" (zoom *. 100.0);
  )
;;

let pivotzoom ?(vw=min state.w state.winw)
      ?(vh=min (state.maxy-state.y) state.winh)
      ?(x=vw/2) ?(y=vh/2) zoom =
  let w = float state.w /. zoom in
  let hw = w /. 2.0 in
  let ratio = float vh /. float vw in
  let hh = hw *. ratio in
  let x0 = float x -. hw
  and y0 = float y -. hh in
  gotoxy (state.x - truncate x0) (state.y + truncate y0);
  setzoom zoom;
;;

let pivotzoom ?vw ?vh ?x ?y zoom =
  if nogeomcmds state.geomcmds
  then
    if zoom > 1.0
    then pivotzoom ?vw ?vh ?x ?y zoom
    else setzoom zoom
;;

let setcolumns mode columns coverA coverB =
  state.prevcolumns <- Some (conf.columns, conf.zoom);
  if columns < 0
  then (
    if isbirdseye mode
    then impmsg "split mode doesn't work in bird's eye"
    else (
      conf.columns <- Csplit (-columns, E.a);
      state.x <- 0;
      conf.zoom <- 1.0;
    );
  )
  else (
    if columns < 2
    then (
      conf.columns <- Csingle E.a;
      state.x <- 0;
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
  state.mode <-
    Birdseye (
        { conf with zoom = conf.zoom },
        state.x, birdseyepageno, -1, getanchor ()
      );
  resetmstate ();
  conf.zoom <- zoom;
  conf.presentation <- false;
  conf.interpagespace <- 10;
  conf.hlinks <- false;
  conf.fitmodel <- FitPage;
  state.x <- 0;
  conf.columns <- (
    match conf.beyecolumns with
    | Some c ->
       conf.zoom <- 1.0;
       Cmulti ((c, 0, 0), E.a)
    | None -> Csingle E.a
  );
  if conf.verbose
  then state.text <- Printf.sprintf "birds eye mode on (zoom %3.1f%%)"
                       (100.0*.zoom)
  else state.text <- E.s;
  reshape state.winw state.winh;
;;

let leavebirdseye (c, leftx, pageno, _, anchor) goback =
  state.mode <- View;
  conf.zoom <- c.zoom;
  conf.presentation <- c.presentation;
  conf.interpagespace <- c.interpagespace;
  conf.hlinks <- c.hlinks;
  conf.fitmodel <- c.fitmodel;
  conf.beyecolumns <- (
    match conf.columns with
    | Cmulti ((c, _, _), _) -> Some c
    | Csingle _ -> None
    | Csplit _ -> error "leaving bird's eye split mode"
  );
  conf.columns <- (
    match c.columns with
    | Cmulti (c, _) -> Cmulti (c, E.a)
    | Csingle _ -> Csingle E.a
    | Csplit (c, _) -> Csplit (c, E.a)
  );
  if conf.verbose
  then state.text <- Printf.sprintf "birds eye mode off (zoom %3.1f%%)"
                       (100.0*.conf.zoom);
  reshape state.winw state.winh;
  state.anchor <- if goback then anchor else (pageno, 0.0, 1.0);
  state.x <- leftx;
;;

let togglebirdseye () =
  match state.mode with
  | Birdseye vals -> leavebirdseye vals true
  | View -> enterbirdseye ()
  | Textentry _ | LinkNav _ -> ()
;;

let upbirdseye incr (conf, leftx, pageno, hooverpageno, anchor) =
  let pageno = max 0 (pageno - incr) in
  let rec loop = function
    | [] -> gotopage1 pageno 0
    | l :: _ when l.pageno = pageno ->
       if l.pagedispy >= 0 && l.pagey = 0
       then postRedisplay "upbirdseye"
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
       let dy = (y - state.y) - (state.winh - h - conf.interpagespace) in
       gotoxy state.x (clamp dy)
    | l :: _ when l.pageno = pageno ->
       if l.pagevh != l.pageh
       then gotoxy state.x (clamp (l.pageh - l.pagevh + conf.interpagespace))
       else postRedisplay "downbirdseye"
    | _ :: rest -> loop rest
  in
  loop state.layout;
  state.text <- E.s;
;;

let optentry mode _ key =
  let btos b = if b then "on" else "off" in
  match [@warning "-4"] key with
  | Keys.Ascii 'C' ->
     let ondone s =
       try
         let n, a, b = multicolumns_of_string s in
         setcolumns mode n a b;
       with exn ->
         state.text <- Printf.sprintf "bad columns `%s': %s" s @@ exntos exn
     in
     TEswitch ("columns: ", E.s, None, textentry, ondone, true)

  | Keys.Ascii 'Z' ->
     let ondone s =
       try
         let zoom = float (int_of_string s) /. 100.0 in
         pivotzoom zoom
       with exn ->
         state.text <- Printf.sprintf "bad integer `%s': %s" s @@ exntos exn
     in
     TEswitch ("zoom: ", E.s, None, intentry, ondone, true)

  | Keys.Ascii 'i' ->
     conf.icase <- not conf.icase;
     TEdone ("case insensitive search " ^ (btos conf.icase))

  | Keys.Ascii 'v' ->
     conf.verbose <- not conf.verbose;
     TEdone ("verbose " ^ (btos conf.verbose))

  | Keys.Ascii 'd' ->
     conf.debug <- not conf.debug;
     TEdone ("debug " ^ (btos conf.debug))

  | Keys.Ascii 'f' ->
     conf.underinfo <- not conf.underinfo;
     TEdone ("underinfo " ^ btos conf.underinfo)

  | Keys.Ascii 'T' ->
     settrim (not conf.trimmargins) conf.trimfuzz;
     TEdone ("trim margins " ^ btos conf.trimmargins)

  | Keys.Ascii 'I' ->
     conf.invert <- not conf.invert;
     TEdone ("invert colors " ^ btos conf.invert)

  | Keys.Ascii 'x' ->
     let ondone s =
       cbput state.hists.sel s;
       conf.selcmd <- s;
     in
     TEswitch ("selection command: ", E.s, Some (onhist state.hists.sel),
               textentry, ondone, true)

  | Keys.Ascii 'M' ->
     if conf.pax == None
     then conf.pax <- Some 0.0
     else conf.pax <- None;
     TEdone ("PAX " ^ btos (conf.pax != None))

  | (Keys.Ascii c) ->
     state.text <- Printf.sprintf "bad option %d `%c'" (Char.code c) c;
     TEstop

  | _ -> TEcont state.text
;;

let adderrmsg src msg =
  Buffer.add_string state.errmsgs msg;
  state.newerrmsgs <- true;
  postRedisplay src
;;

let adderrfmt src fmt = Format.ksprintf (fun s -> adderrmsg src s) fmt;;

class outlinelistview ~zebra ~source =
  let settext autonarrow s =
    if autonarrow
    then
      let ss = source#statestr in
      state.text <- if emptystr ss
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
        postRedisplay "outline navigate";
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
        postRedisplay "outline navscroll";
        coe {< m_first = first; m_active = active >}
      in
      let ctrl = Wsi.withctrl mask in
      let open Keys in
      match Wsi.kc2kt key with
      | Ascii 'a' when ctrl ->
         let text =
           if m_autonarrow
           then (
             source#denarrow;
             E.s
           )
           else (
             let pattern = source#renarrow in
             if nonemptystr m_qsearch
             then (source#narrow m_qsearch; m_qsearch)
             else pattern
           )
         in
         settext (not m_autonarrow) text;
         postRedisplay "toggle auto narrowing";
         coe {< m_first = 0; m_active = 0; m_autonarrow = not m_autonarrow >}

      | Ascii '/' when emptystr m_qsearch && not m_autonarrow ->
         settext true E.s;
         postRedisplay "toggle auto narrowing";
         coe {< m_first = 0; m_active = 0; m_autonarrow = true >}

      | Ascii 'n' when ctrl ->
         source#narrow m_qsearch;
         if not m_autonarrow
         then source#add_narrow_pattern m_qsearch;
         postRedisplay "outline ctrl-n";
         coe {< m_first = 0; m_active = 0 >}

      | Ascii 'S' when ctrl ->
         let active = source#calcactive (getanchor ()) in
         let first = firstof m_first active in
         postRedisplay "outline ctrl-s";
         coe {< m_first = first; m_active = active >}

      | Ascii 'u' when ctrl ->
         postRedisplay "outline ctrl-u";
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

      | Ascii 'l' when ctrl ->
         let first = max 0 (m_active - (fstate.maxrows / 2)) in
         postRedisplay "outline ctrl-l";
         coe {< m_first = first >}

      | Ascii '\t' when m_autonarrow ->
         if nonemptystr m_qsearch
         then (
           postRedisplay "outline list view tab";
           source#add_narrow_pattern m_qsearch;
           settext true E.s;
           coe {< m_qsearch = E.s >}
         )
         else coe self

      | Escape when m_autonarrow ->
         if nonemptystr m_qsearch
         then source#add_narrow_pattern m_qsearch;
         super#key key mask

      | Enter when m_autonarrow ->
         if nonemptystr m_qsearch
         then source#add_narrow_pattern m_qsearch;
         super#key key mask

      | (Ascii _ | Code _) when m_autonarrow ->
         let pattern = m_qsearch ^ toutf8 key in
         postRedisplay "outlinelistview autonarrow add";
         source#narrow pattern;
         settext true pattern;
         coe {< m_first = 0; m_active = 0; m_qsearch = pattern >}

      | Backspace when m_autonarrow ->
         if emptystr m_qsearch
         then coe self
         else
           let pattern = withoutlastutf8 m_qsearch in
           postRedisplay "outlinelistview autonarrow backspace";
           ignore (source#renarrow);
           source#narrow pattern;
           settext true pattern;
           coe {< m_first = 0; m_active = 0; m_qsearch = pattern >}

      | Up when ctrl -> navscroll (max 0 (m_first - 1))

      | Down when ctrl ->
         navscroll (min (source#getitemcount - 1) (m_first + 1))

      | Up    -> navigate ~-1
      | Down  -> navigate 1
      | Prior -> navigate ~-(fstate.maxrows)
      | Next  -> navigate fstate.maxrows

      | Right ->
         let o =
           if ctrl
           then (
             postRedisplay "outline ctrl right";
             {< m_pan = m_pan + 1 >}
           )
           else (
             if Wsi.withshift mask
             then self#nextcurlevel 1
             else self#updownlevel 1
           )
         in
         coe o

      | Left ->
         let o =
           if ctrl
           then (
             postRedisplay "outline ctrl left";
             {< m_pan = m_pan - 1 >}
           )
           else (
             if Wsi.withshift mask
             then self#nextcurlevel ~-1
             else self#updownlevel ~-1
           )
         in
         coe o

      | Home ->
         postRedisplay "outline home";
         coe {< m_first = 0; m_active = 0 >}

      | End ->
         let active = source#getitemcount - 1 in
         let first = max 0 (active - fstate.maxrows) in
         postRedisplay "outline end";
         coe {< m_active = active; m_first = first >}

      | Delete|Escape|Insert|Enter|Ascii _|Code _|Ctrl _|Backspace|Fn _ ->
         super#key key mask
  end;;

let genhistoutlines () =
  Config.gethist ()
  |> List.sort (fun (_, c1, _, _, _, _) (_, c2, _, _, _, _) ->
         compare c2.lastvisit c1.lastvisit)
  |> List.map (fun ((path, c, _, _, _, origin) as hist) ->
         let path = if nonemptystr origin then origin else path in
         let base = mbtoutf8 @@ Filename.basename path in
         (base ^ "\000" ^ c.title, 1, Ohistory hist)
       )
;;

let gotohist (path, c, bookmarks, x, anchor, origin) =
  Config.save leavebirdseye;
  state.anchor <- anchor;
  state.bookmarks <- bookmarks;
  state.origin <- origin;
  state.x <- x;
  setconf conf c;
  Ffi.settrimcachepath conf.trimcachepath;
  let x0, y0, x1, y1 = conf.trimfuzz in
  wcmd "trimset %d %d %d %d %d" (btod conf.trimmargins) x0 y0 x1 y1;
  Wsi.reshape c.cwinw c.cwinh;
  opendoc path origin;
  setzoom c.zoom;
;;

let setcheckers enabled =
  match !checkerstexid with
  | None -> if enabled then checkerstexid := Some (makecheckers ())
  | Some id ->
     if not enabled
     then (
       GlTex.delete_texture id;
       checkerstexid := None;
     );
;;

let describe_layout layout =
  let d =
    match layout with
    | [] -> "Page 0"
    | l :: [] -> Printf.sprintf "Page %d" (l.pageno+1)
    | l :: rest ->
       let rangestr a b =
         if a.pageno = b.pageno then Printf.sprintf "%d" (a.pageno+1)
         else Printf.sprintf "%d%s%d" (a.pageno+1)
                (if a.pageno+1 = b.pageno then ", " else Utf8syms.ellipsis)
                (b.pageno+1)
       in
       let rec fold s la lb = function
         | [] -> Printf.sprintf "%s %s" s (rangestr la lb)
         | l :: rest when l.pageno = succ lb.pageno -> fold s la l rest
         | l :: rest -> fold (s ^ " " ^ rangestr la lb ^ ",") l l rest
       in
       fold "Pages" l l rest
  in
  let percent =
    let maxy = maxy () in
    if maxy <= 0
    then 100.
    else 100. *. (float state.y /. float maxy)
  in
  Printf.sprintf "%s of %d [%.2f%%]" d state.pagecount percent
;;

let setpresentationmode v =
  let n = page_of_y state.y in
  state.anchor <- (n, 0.0, 1.0);
  conf.presentation <- v;
  if conf.fitmodel = FitPage
  then reqlayout conf.angle conf.fitmodel;
  represent ();
;;

let enterinfomode =
  let btos b = if b then Utf8syms.radical else E.s in
  let showextended = ref false in
  let showcolors = ref false in
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
           (name, `int get, 1,
            Action (
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
           (name, `intws get, 1,
            Action (
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
           (name, `color get, 1,
            Action (
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
           (name, `string get, 1,
            Action (
                fun u ->
                let ondone s = set s in
                let te = name ^ ": ", E.s, None, textentry, ondone, true in
                state.mode <- Textentry (te, leave m_prev_mode);
                u
           )) :: m_l

       method colorspace name get set =
         m_l <-
           (name, `string get, 1,
            Action (
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
           (name, `string get, 1,
            Action (
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
           (name, `string get, 1,
            Action (
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
          try set @@ color_of_string v
          with exn ->
            state.text <-
              Printf.sprintf "bad color `%s': %s" v @@ exntos exn
        )
    in
    let rgba name get set =
      src#string name
        (fun () -> get () |> rgba_to_string)
        (fun v ->
          try set @@ rgba_of_string v
          with exn ->
            state.text <-
              Printf.sprintf "bad color `%s': %s" v @@ exntos exn
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

    src#fitmodel "fit model"
      (fun () -> FMTE.to_string conf.fitmodel)
      (fun v -> reqlayout conf.angle (FMTE.of_int v));

    src#bool "trim margins"
      (fun () -> conf.trimmargins)
      (fun v -> settrim v conf.trimfuzz; fillsrc prevmode prevuioh);

    sep ();
    src#int "inter-page space"
      (fun () -> conf.interpagespace)
      (fun n ->
        conf.interpagespace <- n;
        docolumns conf.columns;
        let pageno, py =
          match state.layout with
          | [] -> 0, 0
          | l :: _ -> l.pageno, l.pagey
        in
        state.maxy <- calcheight ();
        let y = getpagey pageno in
        gotoxy state.x (y + py)
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
      (fun () ->
        Printf.sprintf "%s bytes, %d tiles"
          (string_with_suffix_of_int state.memused)
          (Hashtbl.length state.tilemap)) 1;

    sep ();
    src#caption "Layout" 0;
    src#caption2 "Dimension"
      (fun () -> Printf.sprintf "%dx%d (virtual %dx%d)"
                   state.winw state.winh
                   state.w state.maxy)
      1;
    if conf.debug
    then src#caption2 "Position" (fun () ->
             Printf.sprintf "%dx%d" state.x state.y
           ) 1
    else src#caption2 "Position" (fun () -> describe_layout state.layout) 1;

    sep ();
    src#bool ~offset:0 ~btos:(fun v -> if v then "(on)" else "(off)")
      "Save these parameters as global defaults at exit"
      (fun () -> conf.bedefault)
      (fun v -> conf.bedefault <- v);

    sep ();
    let btos b = Utf8syms.(if b then lguillemet else rguillemet) in
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
          then conf.pax <- Some (now ())
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
          if Ffi.realloctexts v
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
          try conf.pgscale <- float_of_string v
          with exn ->
            state.text <-
              Printf.sprintf "bad page scroll scaling factor `%s': %s" v
              @@ exntos exn
        );
      src#int "ui font size"
        (fun () -> fstate.fontsize)
        (fun v -> setfontsize (bound v 5 100));
      src#int "hint font size"
        (fun () -> conf.hfsize)
        (fun v -> conf.hfsize <- bound v 5 100);
      src#string "trim fuzz"
        (fun () -> irect_to_string conf.trimfuzz)
        (fun v ->
          try
            conf.trimfuzz <- irect_of_string v;
            if conf.trimmargins
            then settrim true conf.trimfuzz;
          with exn ->
            state.text <- Printf.sprintf "bad irect `%s': %s" v
                          @@ exntos exn
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
      if Ffi.bousable ()
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
      src#bool "use document CSS"
        (fun () -> conf.usedoccss)
        (fun v ->
          conf.usedoccss <- v;
          state.anchor <- getanchor ();
          opendoc state.path state.password;
        );
      src#bool ~btos "colors"
        (fun () -> !showcolors)
        (fun v -> showcolors := v; fillsrc prevmode prevuioh);
      if !showcolors
      then (
        colorp "   background"
          (fun () -> conf.bgcolor)
          (fun v -> conf.bgcolor <- v);

        rgba "   paper color"
          (fun () -> conf.papercolor)
          (fun v ->
            conf.papercolor <- v;
            Ffi.setpapercolor conf.papercolor;
            flushtiles ();
          );
        rgba "   scrollbar"
          (fun () -> conf.sbarcolor)
          (fun v -> conf.sbarcolor <- v);
        rgba "   scrollbar handle"
          (fun () -> conf.sbarhndlcolor)
          (fun v -> conf.sbarhndlcolor <- v);
        rgba "   texture color"
          (fun () -> conf.texturecolor)
          (fun v ->
            GlTex.env (`color v);
            conf.texturecolor <- v;
          )
      );
    );

    sep ();
    src#caption "Document" 0;
    List.iter (fun (_, s) -> src#caption s 1) state.docinfo;
    src#caption2 "Pages" (fun () ->  string_of_int state.pagecount) 1;
    src#caption2 "Dimensions"
      (fun () -> string_of_int (List.length state.pdims)) 1;
    if nonemptystr conf.css
    then src#caption2 "CSS" (fun () -> conf.css) 1;
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
  fun () -> (
    state.text <- E.s;
    resetmstate ();
    let prevmode = state.mode
    and prevuioh = state.uioh in
    fillsrc prevmode prevuioh;
    let source = (src :> lvsource) in
    let modehash = findkeyhash conf "info" in
    state.uioh <-
      coe (object (self)
             inherit listview ~zebra:false ~helpmode:false
                       ~source ~trusted:true ~modehash as super
             val mutable m_prevmemused = 0
             method! infochanged = function
               | Memused ->
                  if m_prevmemused != state.memused
                  then (
                    m_prevmemused <- state.memused;
                    postRedisplay "memusedchanged";
                  )
               | Pdim -> postRedisplay "pdimchanged"
               | Docinfo -> fillsrc prevmode prevuioh

             method! key key mask =
               if not (Wsi.withctrl mask)
               then
                 match [@warning "-4"] Wsi.kc2kt key with
                 | Keys.Left  -> coe (self#updownlevel ~-1)
                 | Keys.Right -> coe (self#updownlevel 1)
                 | _ -> super#key key mask
               else super#key key mask
           end);
    postRedisplay "info";
  );
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
  in
  fun () ->
  let modehash = findkeyhash conf "help" in
  resetmstate ();
  state.uioh <- coe (new listview
                       ~zebra:false ~helpmode:true
                       ~source ~trusted:true ~modehash);
  postRedisplay "help";
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
         let l = Str.split Utils.Re.crlf (Buffer.contents state.errmsgs) in
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
     state.uioh <-
       coe (object
             inherit listview ~zebra:false ~helpmode:false
                       ~source ~trusted:false ~modehash as super
             method! display =
               if state.newerrmsgs
               then msgsource#reset;
               super#display
           end);
     postRedisplay "msgs";
;;

let getusertext s =
  let editor = getenvdef "EDITOR" E.s in
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
      | exception exn ->
         impmsg "spawn(%S) failed: %s" execstr @@ exntos exn;
         E.s
      | pid ->
         match Unix.waitpid [] pid with
         | exception exn ->
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
    | exception exn ->
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
           then (String.sub s b (p-b), fun () -> ()) :: accu
           else
             if (i > 70 && s.[p] = ' ') || s.[p] = '\r' || s.[p] = '\n'
             then
               let ss = if i = 0 then E.s else String.sub s b i in
               split ((ss, fun () -> ())::accu) (p+1) 0
             else split accu b (i+1)
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
           Ffi.delannot opaque slinkindex;
           cleanup ();
         in
         let edit inline () =
           let update s =
             if emptystr s
             then dele ()
             else (
               Ffi.modannot opaque slinkindex s;
               cleanup ();
             )
           in
           if inline
           then
             let mode = state.mode in
             state.mode <-
               Textentry (
                   ("annotation: ", m_text, None, textentry, update, true),
                   fun _ -> state.mode <- mode
                 );
             state.text <- E.s;
             enttext ();
           else
             let s = getusertext m_text in
             update s
         in
         m_text <- s;
         m_items <-
           (   "[Copy]", fun () -> selstring conf.selcmd m_text)
           :: ("[Delete]", dele)
           :: ("[Edit]", edit conf.annotinline)
           :: (E.s, fun () -> ())
           :: split [] 0 0 |> List.rev |> Array.of_list

       initializer
         m_active <- 0
     end)
  in
  state.text <- E.s;
  let s = Ffi.getannotcontents opaque slinkindex in
  resetmstate ();
  msgsource#reset s;
  let source = (msgsource :> lvsource) in
  let modehash = findkeyhash conf "listview" in
  state.uioh <- coe (object
                      inherit listview ~zebra:false ~helpmode:false
                                ~source ~trusted:false ~modehash
                    end);
  postRedisplay "enterannotmode";
;;

let gotoremote spec =
  let filename, dest = splitatchar spec '#' in
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
  let path = getpath filename in
  let dospawn lcmd =
    if conf.riani
    then
      let cmd = Lazy.force_val lcmd in
      match spawn cmd with
      | _pid -> ()
      | exception exn -> dolog "failed to execute `%s': %s" cmd @@ exntos exn
    else
      let anchor = getanchor () in
      let ranchor = state.path, state.password, anchor, state.origin in
      state.origin <- E.s;
      state.ranchors <- ranchor :: state.ranchors;
      opendoc path E.s;
  in
  if substratis spec 0 "page="
  then
    match Scanf.sscanf spec "page=%d" (fun n -> n) with
    | pageno ->
       state.anchor <- (pageno, 0.0, 0.0);
       dospawn @@ lazy (Printf.sprintf "%s -page %d %S" !selfexec pageno path);
    | exception exn ->
       adderrfmt "error parsing remote destination" "page: %s" @@ exntos exn
  else (
    state.nameddest <- dest;
    dospawn @@ lazy (!selfexec ^ " " ^ path ^ " -dest " ^ dest)
  )
;;

let gotounder = function
  | Ulinkuri s when Ffi.isexternallink s ->
     if substratis s 0 "file://"
     then gotoremote @@ String.sub s 7 (String.length s - 7)
     else Help.gotouri conf.urilauncher s
  | Ulinkuri s ->
     let pageno, x, y = Ffi.uritolocation s in
     addnav ();
     gotopagexy pageno x y
  | Utext _ | Unone -> ()
  | Uannotation (opaque, slinkindex) -> enterannotmode opaque slinkindex
;;

let gotooutline (_, _, kind) =
  match kind with
  | Onone -> ()
  | Oanchor ((pageno, y, _) as anchor) ->
     addnav ();
     gotoxy state.x @@
       getanchory (if conf.presentation then (pageno, y, 1.0) else anchor)
  | Ouri uri -> gotounder (Ulinkuri uri)
  | Olaunch cmd -> error "gotounder (Ulaunch %S)" cmd
  | Oremote (remote, pageno) ->
     error "gotounder (Uremote (%S,%d) )" remote pageno
  | Ohistory hist -> gotohist hist
  | Oremotedest (path, dest) ->
     error "gotounder (Uremotedest (%S, %S))" path dest
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

  method exit ~(uioh:uioh) ~cancel ~active ~(first:int) ~pan : uioh option =
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
        | many -> String.concat Utf8syms.ellipsis (List.rev many)
      in
      "Narrowed to " ^ s ^ " (ctrl-u to restore)"
    else E.s

  method statestr =
    match m_narrow_patterns with
    | [] -> E.s
    | one :: [] -> one
    | head :: _ -> Utf8syms.ellipsis ^ head

  method narrow pattern =
    match Str.regexp_case_fold pattern with
    | exception _ -> ()
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
             | exception Not_found -> accu, minfo
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
           pattern ^ Utf8syms.ellipsis ^ accu) E.s list

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
              let d = abs (orely - rely) in
              if d < bestd
              then loop (n+1) n d
              else loop (n+1) best bestd
           | Onone | Oremote _ | Olaunch _
           | Oremotedest _ | Ouri _ | Ohistory _ ->
              loop (n+1) best bestd
       in
       loop 0 ~-1 max_int
   end)
;;

let enteroutlinemode, enterbookmarkmode, enterhistmode =
  let fetchoutlines sourcetype () =
    match sourcetype with
    | `bookmarks -> Array.of_list state.bookmarks
    | `outlines -> state.outlines
    | `history -> genhistoutlines () |> Array.of_list
  in
  let so = outlinesource (fetchoutlines `outlines) in
  let sb = outlinesource (fetchoutlines `bookmarks) in
  let sh = outlinesource (fetchoutlines `history) in
  let mkselector sourcetype source =
    (fun errmsg ->
      let outlines = fetchoutlines sourcetype () in
      if Array.length outlines = 0
      then showtext ' ' errmsg
      else (
        resetmstate ();
        Wsi.setcursor Wsi.CURSOR_INHERIT;
        let anchor = getanchor () in
        source#reset anchor outlines;
        state.text <- source#greetmsg;
        state.uioh <-
          coe (new outlinelistview ~zebra:(sourcetype=`history) ~source);
        postRedisplay "enter selector";
      )
    )
  in
  let mkenter sourcetype errmsg s = fun () -> mkselector sourcetype s errmsg in
  ( mkenter `outlines "document has no outline" so
  , mkenter `bookmarks "document has no bookmarks (yet)" sb
  , mkenter `history "history is empty" sh )
;;


let addbookmark title a =
  let b = List.filter (fun (title', _, _) -> title <> title') state.bookmarks in
  state.bookmarks <- (title, 0, Oanchor a) :: b;;

let quickbookmark ?title () =
  match state.layout with
  | [] -> ()
  | l :: _ ->
     let title =
       match title with
       | None ->
          Unix.(
           let tm = localtime (now ()) in
           Printf.sprintf
             "Quick (page %d) (bookmarked on %02d/%02d/%d at %02d:%02d)"
             (l.pageno+1)
             tm.tm_mday (tm.tm_mon+1) (tm.tm_year+1900) tm.tm_hour tm.tm_min
          )
       | Some title -> title
     in
     addbookmark title (getanchor1 l)
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
  | Csingle _ | Cmulti _ -> state.x != 0 || conf.zoom > 1.0
;;

let panbound x = bound x (-state.w) state.winw;;

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
     gotoxy state.x (getpagey (pageno+1))
  | l :: rest ->
     match conf.columns with
     | Csingle _ ->
        if conf.presentation && rest == [] && l.pageh > l.pagey + l.pagevh
        then
          let y = clamp (pgscale state.winh) in
          gotoxy state.x y
        else
          let pageno = min (l.pageno+1) (state.pagecount-1) in
          gotoxy state.x (getpagey pageno)
     | Cmulti ((c, _, _) as cl, _) ->
        if conf.presentation
           && (existsinrow l.pageno cl
                 (fun l -> l.pageh > l.pagey + l.pagevh))
        then
          let y = clamp (pgscale state.winh) in
          gotoxy state.x y
        else
          let pageno = min (l.pageno+c) (state.pagecount-1) in
          gotoxy state.x (getpagey pageno)
     | Csplit (n, _) ->
        if l.pageno < state.pagecount - 1 || l.pagecol < n - 1
        then
          let pagey, pageh = getpageyh l.pageno in
          let pagey = pagey + pageh * l.pagecol in
          let ips = if l.pagecol = 0 then 0 else conf.interpagespace in
          gotoxy state.x (pagey + pageh + ips)
;;

let prevpage () =
  match state.layout with
  | [] ->
     let pageno = page_of_y state.y in
     gotoxy state.x (getpagey (pageno-1))
  | l :: _ ->
     match conf.columns with
     | Csingle _ ->
        if conf.presentation && l.pagey != 0
        then gotoxy state.x (clamp (pgscale ~-(state.winh)))
        else
          let pageno = max 0 (l.pageno-1) in
          gotoxy state.x (getpagey pageno)
     | Cmulti ((c, _, coverB) as cl, _) ->
        if conf.presentation &&
             (existsinrow l.pageno cl (fun l -> l.pagey != 0))
        then gotoxy state.x (clamp (pgscale ~-(state.winh)))
        else
          let decr =
            if l.pageno = state.pagecount - coverB
            then 1
            else c
          in
          let pageno = max 0 (l.pageno-decr) in
          gotoxy state.x (getpagey pageno)
     | Csplit (n, _) ->
        let y =
          if l.pagecol = 0
          then
            if l.pageno = 0
            then l.pagey
            else
              let pageno = max 0 (l.pageno-1) in
              let pagey, pageh = getpageyh pageno in
              pagey + (n-1)*pageh
          else
            let pagey, pageh = getpageyh l.pageno in
            pagey + pageh * (l.pagecol-1) - conf.interpagespace
        in
        gotoxy state.x y
;;

let save () =
  if emptystr conf.savecmd
  then adderrmsg "savepath-command is empty"
         "don't know where to save modified document"
  else
    let savecmd = Str.global_replace Utils.Re.percent state.path conf.savecmd in
    let path =
      getcmdoutput
        (fun exn ->
          adderrfmt savecmd "failed to produce path to the saved copy: %s" exn)
        savecmd
    in
    if nonemptystr path
    then
      let tmp = path ^ ".tmp" in
      Ffi.savedoc tmp;
      Unix.rename tmp path;
;;

let viewkeyboard key mask =
  let enttext te =
    let mode = state.mode in
    state.mode <- Textentry (te, fun _ -> state.mode <- mode);
    state.text <- E.s;
    enttext ();
    postRedisplay "view:enttext"
  in
  let ctrl = Wsi.withctrl mask in
  let open Keys in
  match Wsi.kc2kt key with
  | Ascii 'S' -> state.slideshow <- state.slideshow lxor 1

  | Ascii 'Q' -> exit 0

  | Ascii 'z' ->
     let yloc f =
       match List.rev state.rects with
       | [] -> ()
       | (pageno, _, (_, y0, _, y1, _, y2, _, y3)) :: _ ->
          f pageno (y0, y1, y2, y3)
     and yminmax (y0, y1, y2, y3) =
       let ym = min y0 y1 |> min y2 |> min y3 |> truncate in
       let yM = max y0 y1 |> max y2 |> max y3 |> truncate in
       ym, yM
     in
     let ondone msg = state.text <- msg
     and zmod _ _ k =
       match [@warning "-4"] k with
       | Keys.Ascii 'z' ->
          let f pageno ys =
            let ym, yM = yminmax ys in
            let hh = (yM - ym)/2 in
            gotopage1 pageno (ym + hh - state.winh/2)
          in
          yloc f;
          TEdone "center"
       | Keys.Ascii 't' ->
          let f pageno ys =
            let ym, _ = yminmax ys in
            gotopage1 pageno ym
          in
          yloc f;
          TEdone "top"
       | Keys.Ascii 'b' ->
          let f pageno ys =
            let _, yM = yminmax ys in
            gotopage1 pageno (yM - state.winh)
          in
          yloc f;
          TEdone "bottom"
       | _ -> TEstop
     in
     enttext (": ", E.s, None, zmod state.mode, ondone, true)

  | Ascii 'W' ->
     if Ffi.hasunsavedchanges ()
     then save ()

  | Insert ->
     if conf.angle mod 360 = 0 && not (isbirdseye state.mode)
     then (
       state.mode <- (
         match state.lnava with
         | None -> LinkNav (Ltgendir 0)
         | Some pn -> LinkNav (Ltexact pn)
       );
       gotoxy state.x state.y;
     )
     else impmsg "keyboard link navigation does not work under rotation"

  | Escape | Ascii 'q' ->
     begin match state.mstate with
     | Mzoomrect _ ->
        resetmstate ();
        postRedisplay "kill rect";
     | Msel _
     | Mpan _
     | Mscrolly | Mscrollx
     | Mzoom _
     | Mnone ->
        begin match state.mode with
        | LinkNav ln ->
           begin match ln with
           | Ltexact pl -> state.lnava <- Some pl
           | Ltgendir _ | Ltnotready _ -> state.lnava <- None
           end;
           state.mode <- View;
           postRedisplay "esc leave linknav"
        | Birdseye _ | Textentry _ | View ->
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

  | Ascii 'o' -> enteroutlinemode ()
  | Ascii 'H' -> enterhistmode ()

  | Ascii 'u' ->
     state.rects <- [];
     state.text <- E.s;
     Hashtbl.iter (fun _ opaque ->
         Ffi.clearmark opaque;
         Hashtbl.clear state.prects) state.pagemap;
     postRedisplay "dehighlight";

  | Ascii (('/' | '?') as c) ->
     let ondone isforw s =
       cbput state.hists.pat s;
       state.searchpattern <- s;
       search s isforw
     in
     let s = String.make 1 c in
     enttext (s, E.s, Some (onhist state.hists.pat),
              textentry, ondone (c = '/'), true)

  | Ascii '+' | Ascii '=' when ctrl ->
     let incr = if conf.zoom +. 0.01 > 0.1 then 0.1 else 0.01 in
     pivotzoom (conf.zoom +. incr)

  | Ascii '+' ->
     let ondone s =
       let n =
         try int_of_string s with exn ->
           state.text <-
             Printf.sprintf "bad integer `%s': %s" s @@ exntos exn;
           max_int
       in
       if n != max_int
       then (
         conf.pagebias <- n;
         state.text <- "page bias is now " ^ string_of_int n;
       )
     in
     enttext ("page bias: ", E.s, None, intentry, ondone, true)

  | Ascii '-' when ctrl ->
     let decr = if conf.zoom -. 0.1 < 0.1 then 0.01 else 0.1 in
     pivotzoom (max 0.01 (conf.zoom -. decr))

  | Ascii '-' ->
     let ondone msg = state.text <- msg in
     enttext ("option: ", E.s, None,
              optentry state.mode, ondone, true)

  | Ascii '0' when ctrl ->
     if conf.zoom = 1.0
     then gotoxy 0 state.y
     else setzoom 1.0

  | Ascii ('1'|'2' as c) when ctrl && conf.fitmodel != FitPage ->
     let cols =
       match conf.columns with
       | Csingle _ | Cmulti _ -> 1
       | Csplit (n, _) -> n
     in
     let h = state.winh -
               conf.interpagespace lsl (if conf.presentation then 1 else 0)
     in
     let zoom = Ffi.zoomforh state.winw h 0 cols in
     if zoom > 0.0 && (c = '2' || zoom < 1.0)
     then setzoom zoom

  | Ascii '3' when ctrl ->
     let fm =
       match conf.fitmodel with
       | FitWidth -> FitProportional
       | FitProportional -> FitPage
       | FitPage -> FitWidth
     in
     state.text <- "fit model: " ^ FMTE.to_string fm;
     reqlayout conf.angle fm

  | Ascii '4' when ctrl ->
     let zoom = Ffi.getmaxw () /. float state.winw in
     if zoom > 0.0 then setzoom zoom

  | Fn 9 | Ascii '9' when ctrl -> togglebirdseye ()

  | Ascii ('0'..'9' as c) when not ctrl ->
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
     let pageentry text = function [@warning "-4"]
                                 | Keys.Ascii 'g' -> TEdone text
                                 | key -> intentry text key
     in
     let text = String.make 1 c in
     enttext (":", text, Some (onhist state.hists.pag),
              pageentry, ondone, true)

  | Ascii 'b' ->
     conf.scrollb <- if conf.scrollb = 0 then (scrollbvv lor scrollbhv) else 0;
     postRedisplay "toggle scrollbar";

  | Ascii 'B' ->
     state.bzoom <- not state.bzoom;
     state.rects <- [];
     showtext ' ' ("block zoom " ^ if state.bzoom then "on" else "off")

  | Ascii 'l' ->
     conf.hlinks <- not conf.hlinks;
     state.text <- "highlightlinks " ^ if conf.hlinks then "on" else "off";
     postRedisplay "toggle highlightlinks";

  | Ascii 'F' ->
     if conf.angle mod 360 = 0
     then (
       state.glinks <- true;
       let mode = state.mode in
       state.mode <-
         Textentry (
             ("goto: ", E.s, None, linknentry, linknact gotounder, false),
             (fun _ ->
               state.glinks <- false;
               state.mode <- mode)
           );
       state.text <- E.s;
       postRedisplay "view:linkent(F)"
     )
     else impmsg "hint mode does not work under rotation"

  | Ascii 'y' ->
     state.glinks <- true;
     let mode = state.mode in
     state.mode <-
       Textentry (
           ("copy: ", E.s, None, linknentry,
            linknact (fun under ->
                selstring conf.selcmd (undertext under)), false),
           (fun _ ->
             state.glinks <- false;
             state.mode <- mode)
         );
     state.text <- E.s;
     postRedisplay "view:linkent"

  | Ascii 'a' ->
     begin match state.autoscroll with
     | Some step ->
        conf.autoscrollstep <- step;
        state.autoscroll <- None
     | None ->
        state.autoscroll <- Some conf.autoscrollstep;
        state.slideshow <- state.slideshow land lnot 2
     end

  | Ascii 'p' when ctrl ->
     launchpath ()              (* XXX where do error messages go? *)

  | Ascii 'P' ->
     setpresentationmode (not conf.presentation);
     showtext ' ' ("presentation mode " ^
                     if conf.presentation then "on" else "off");

  | Ascii 'f' ->
     if List.mem Wsi.Fullscreen state.winstate
     then Wsi.reshape conf.cwinw conf.cwinh
     else Wsi.fullscreen ()

  | Ascii ('p'|'N') -> search state.searchpattern false
  | Ascii 'n' | Fn 3 -> search state.searchpattern true

  | Ascii 't' ->
     begin match state.layout with
     | [] -> ()
     | l :: _ -> gotoxy state.x (getpagey l.pageno)
     end

  | Ascii ' ' -> nextpage ()
  | Delete -> prevpage ()
  | Ascii '=' -> showtext ' ' (describe_layout state.layout);

  | Ascii 'w' ->
     begin match state.layout with
     | [] -> ()
     | l :: _ ->
        Wsi.reshape l.pagew l.pageh;
        postRedisplay "w"
     end

  | Ascii '\'' -> enterbookmarkmode ()
  | Ascii 'h' | Fn 1 -> enterhelpmode ()
  | Ascii 'i' -> enterinfomode ()
  | Ascii 'e' when Buffer.length state.errmsgs > 0 -> entermsgsmode ()

  | Ascii 'm' ->
     let ondone s =
       match state.layout with
       | l :: _ when nonemptystr s -> addbookmark s @@ getanchor1 l
       | _ -> ()
     in
     enttext ("bookmark: ", E.s, None, textentry, ondone, true)

  | Ascii '~' ->
     quickbookmark ();
     showtext ' ' "Quick bookmark added";

  | Ascii 'x' -> state.roam ()

  | Ascii ('<'|'>' as c) ->
     reqlayout (conf.angle + (if c = '>' then 30 else -30)) conf.fitmodel

  | Ascii ('['|']' as c) ->
     conf.colorscale <-
       bound (conf.colorscale +. (if c = ']' then 0.1 else -0.1)) 0.0 1.0;
     postRedisplay "brightness";

  | Ascii 'c' when state.mode = View ->
     if Wsi.withalt mask
     then (
       if conf.zoom > 1.0
       then
         let m = (state.winw - state.w) / 2 in
         gotoxy m state.y
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

  | Down | Up when ctrl && Wsi.withshift mask ->
     let zoom, x = state.prevzoom in
     setzoom zoom;
     state.x <- x;

  | Up ->
     begin match state.autoscroll with
     | None ->
        begin match state.mode with
        | Birdseye beye -> upbirdseye 1 beye
        | Textentry _ | View | LinkNav _ ->
           if ctrl
           then gotoxy state.x (clamp ~-(state.winh/2))
           else (
             if not (Wsi.withshift mask) && conf.presentation
             then prevpage ()
             else gotoxy state.x (clamp (-conf.scrollstep))
           )
        end
     | Some n -> setautoscrollspeed n false
     end

  | Down ->
     begin match state.autoscroll with
     | None ->
        begin match state.mode with
        | Birdseye beye -> downbirdseye 1 beye
        | Textentry _ | View | LinkNav _ ->
           if ctrl
           then gotoxy state.x (clamp (state.winh/2))
           else (
             if not (Wsi.withshift mask) && conf.presentation
             then nextpage ()
             else gotoxy state.x (clamp (conf.scrollstep))
           )
        end
     | Some n -> setautoscrollspeed n true
     end

  | Left | Right when not (Wsi.withalt mask) ->
     if canpan ()
     then
       let dx =
         if ctrl
         then state.winw / 2
         else conf.hscrollstep
       in
       let dx =
         let pv = Wsi.kc2kt key in
         if pv = Keys.Left then dx else -dx
       in
       gotoxy (panbound (state.x + dx)) state.y
     else (
       state.text <- E.s;
       postRedisplay "left/right"
     )

  | Prior ->
     let y =
       if ctrl
       then
         match state.layout with
         | [] -> state.y
         | l :: _ -> state.y - l.pagey
       else clamp (pgscale (-state.winh))
     in
     gotoxy state.x y

  | Next ->
     let y =
       if ctrl
       then
         match List.rev state.layout with
         | [] -> state.y
         | l :: _ -> getpagey l.pageno
       else clamp (pgscale state.winh)
     in
     gotoxy state.x y

  | Ascii 'g' | Home ->
     addnav ();
     gotoxy 0 0
  | Ascii 'G' | End ->
     addnav ();
     gotoxy 0 (clamp state.maxy)

  | Right when Wsi.withalt mask ->
     let nav = state.nav in
     (match nav.future with
     | [] -> ()
     | next :: frest ->
        state.nav <-
          { past = getanchor () :: nav.past
          ; future = frest
          };
        gotoxy state.x (getanchory next)
     )
  | Backspace | Left when Wsi.withalt mask ->
     let nav = state.nav in
     (match nav.past with
     | [] -> ()
     | prev :: prest ->
        state.nav <-
          { past = prest
          ; future = getanchor () :: nav.future
          };
        gotoxy state.x (getanchory prev)
     )

  | Ascii 'r' ->
     reload ()

  | Ascii 'v' when conf.debug ->
     state.rects <- [];
     List.iter (fun l ->
         match getopaque l.pageno with
         | None -> ()
         | Some opaque ->
            let x0, y0, x1, y1 = Ffi.pagebbox opaque in
            let rect = (float x0, float y0,
                        float x1, float y0,
                        float x1, float y1,
                        float x0, float y1) in
            debugrect rect;
            let color = (0.0, 0.0, 1.0 /. (l.pageno mod 3 |> float), 0.5) in
            state.rects <- (l.pageno, color, rect) :: state.rects;
       ) state.layout;
     postRedisplay "v";

  | Ascii '|' ->
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
     postRedisplay "|";
     state.mode <- Textentry (te, onleave);

  | (Ascii _|Fn _|Enter|Left|Right|Code _|Ctrl _) ->
     vlog "huh? %s" (Wsi.keyname key)
;;

let linknavkeyboard key mask linknav =
  let pv = Wsi.kc2kt key in
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
       if pv = Keys.Enter
       then
         let under = Ffi.getlink opaque n in
         postRedisplay "link gotounder";
         gotounder under;
         state.mode <- View;
       else
         let opt, dir =
           let open Keys in
           match pv with
           | Home -> Some (Ffi.findlink opaque LDfirst), -1
           | End -> Some (Ffi.findlink opaque LDlast), 1
           | Left -> Some (Ffi.findlink opaque (LDleft n)), -1
           | Right -> Some (Ffi.findlink opaque (LDright n)), 1
           | Up -> Some (Ffi.findlink opaque (LDup n)), -1
           | Down -> Some (Ffi.findlink opaque (LDdown n)), 1
           | Delete|Escape|Insert|Enter|Next|Prior|Ascii _
           | Code _|Fn _|Ctrl _|Backspace -> None, 0
         in
         let pwl l dir =
           begin match Ffi.findpwl l.pageno dir with
           | Pwlnotfound -> ()
           | Pwl pageno ->
              let notfound dir =
                state.mode <- LinkNav (Ltgendir dir);
                let y, h = getpageyh pageno in
                let y =
                  if dir < 0
                  then y + h - state.winh
                  else y
                in
                gotoxy state.x y
              in
              begin match getopaque pageno, getpage pageno with
              | Some opaque, Some _ ->
                 let link =
                   let ld = if dir > 0 then LDfirst else LDlast in
                   Ffi.findlink opaque ld
                 in
                 begin match link with
                 | Lfound m ->
                    showlinktype (Ffi.getlink opaque m);
                    state.mode <- LinkNav (Ltexact (pageno, m));
                    postRedisplay "linknav jpage";
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
              let _, y0, _, y1 = Ffi.getlinkrect opaque m in
              if y0 < l.pagey
              then gotopage1 l.pageno y0
              else (
                let d = fstate.fontsize + 1 in
                if y1 - l.pagey > l.pagevh - d
                then gotopage1 l.pageno (y1 - state.winh + d)
                else postRedisplay "linknav";
              );
              showlinktype (Ffi.getlink opaque m);
              state.mode <- LinkNav (Ltexact (l.pageno, m));
            )

         | None -> viewkeyboard key mask
         end;
    | _ -> viewkeyboard key mask
  in
  if pv = Keys.Insert
  then (
    begin match linknav with
    | Ltexact pa -> state.lnava <- Some pa
    | Ltgendir _ | Ltnotready _ -> ()
    end;
    state.mode <- View;
    postRedisplay "leave linknav"
  )
  else
    match linknav with
    | Ltgendir _ | Ltnotready _ -> viewkeyboard key mask
    | Ltexact exact -> doexact exact
;;

let keyboard key mask =
  if (key = Char.code 'g' && Wsi.withctrl mask) && not (istextentry state.mode)
  then wcmd "interrupt"
  else state.uioh <- state.uioh#key key mask
;;

let birdseyekeyboard key mask
      ((oconf, leftx, pageno, hooverpageno, anchor) as beye) =
  let incr =
    match conf.columns with
    | Csingle _ -> 1
    | Cmulti ((c, _, _), _) -> c
    | Csplit _ -> error "bird's eye split mode"
  in
  let pgh layout = List.fold_left
                     (fun m l -> max l.pageh m) state.winh layout in
  let open Keys in
  match Wsi.kc2kt key with
  | Ascii 'l' when Wsi.withctrl mask ->
     let y, h = getpageyh pageno in
     let top = (state.winh - h) / 2 in
     gotoxy state.x (max 0 (y - top))
  | Enter -> leavebirdseye beye false
  | Escape -> leavebirdseye beye true
  | Up -> upbirdseye incr beye
  | Down -> downbirdseye incr beye
  | Left -> upbirdseye 1 beye
  | Right -> downbirdseye 1 beye

  | Prior ->
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
          let layout = layout state.x (state.y-state.winh)
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

  | Next ->
     begin match List.rev state.layout with
     | l :: _ ->
        let layout = layout state.x
                       (state.y + (pgh state.layout))
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
             postRedisplay "birdseye pagedown";
           )
           else gotoxy state.x (clamp (incr + conf.interpagespace*2));

        | l :: _ ->
           state.mode <-
             Birdseye (oconf, leftx, l.pageno, hooverpageno, anchor);
           gotopage1 l.pageno 0;
        end

     | [] -> gotoxy state.x (clamp state.winh)
     end;

  | Home ->
     state.mode <- Birdseye (oconf, leftx, 0, hooverpageno, anchor);
     gotopage1 0 0

  | End ->
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
         (max 0 (getpagey pageno - (state.winh - h - conf.interpagespace)))
     else postRedisplay "birdseye end";

  | Delete|Insert|Ascii _|Code _|Ctrl _|Fn _|Backspace -> viewkeyboard key mask
;;

let drawpage l =
  let color =
    match state.mode with
    | Textentry _ -> scalecolor 0.4
    | LinkNav _ | View -> scalecolor 1.0
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
             (float (l.pagedispy+l.pagevh+1));
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
         List.iter (fun vals -> Ffi.drawprect opaque x y vals);
       let n =
         Ffi.postprocess opaque hlmask x y (linkindexbase, s, conf.hfsize) in
       if n < 0
       then (Glutils.redisplay := true; 0)
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
  GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
  let (r, g, b, alpha) = conf.sbarcolor in
  GlDraw.color (r, g, b) ~alpha;
  filledrect (float x0) 0. (float x1) (float state.winh);
  filledrect
    (float hx0) (float (state.winh - sbh))
    (float (hx0 + state.winw)) (float state.winh);
  let (r, g, b, alpha) = conf.sbarhndlcolor in
  GlDraw.color (r, g, b) ~alpha;

  filledrect (float x0) ph (float x1) (ph +. sh);
  let pw = pw +. float hx0 in
  filledrect pw (float (state.winh - sbh)) (pw +. sw) (float state.winh);
  Gl.disable `blend;
;;

let showsel () =
  match state.mstate with
  | Mnone | Mscrolly | Mscrollx | Mpan _ | Mzoom _ | Mzoomrect _ -> ()
  | Msel ((x0, y0), (x1, y1)) ->
     let identify opaque l px py = Some (opaque, l.pageno, px, py) in
     let o0,n0,px0,py0 = onppundermouse identify x0 y0 (~< E.s, -1, 0, 0) in
     let _o1,n1,px1,py1 = onppundermouse identify x1 y1 (~< E.s, -1, 0, 0) in
     if n0 != -1 && n0 = n1 then Ffi.seltext o0 (px0, py0, px1, py1);
;;

let showrects = function
  | [] -> ()
  | rects ->
     Gl.enable `blend;
     GlDraw.color (0.0, 0.0, 1.0) ~alpha:0.5;
     GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
     List.iter
       (fun (pageno, c, (x0, y0, x1, y1, x2, y2, x3, y3)) ->
         List.iter (fun l ->
             if l.pageno = pageno
             then
               let dx = float (l.pagedispx - l.pagex) in
               let dy = float (l.pagedispy - l.pagey) in
               let r, g, b, alpha = c in
               GlDraw.color (r, g, b) ~alpha;
               filledrect2
                 (x0+.dx) (y0+.dy)
                 (x1+.dx) (y1+.dy)
                 (x3+.dx) (y3+.dy)
                 (x2+.dx) (y2+.dy);
           ) state.layout
       ) rects;
     Gl.disable `blend;
;;

let display () =
  GlDraw.color (scalecolor2 conf.bgcolor);
  GlClear.color (scalecolor2 conf.bgcolor);
  GlClear.clear [`color];
  List.iter drawpage state.layout;
  let rects =
    match state.mode with
    | LinkNav (Ltexact (pageno, linkno)) ->
       begin match getopaque pageno with
       | Some opaque ->
          let x0, y0, x1, y1 = Ffi.getlinkrect opaque linkno in
          let color =
            if conf.invert
            then (1.0, 1.0, 1.0, 0.5)
            else (0.0, 0.0, 0.5, 0.5)
          in
          (pageno, color,
           (float x0, float y0,
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

let display () =
  match state.reload with
  | Some (x, y, t) ->
     if x != state.x || y != state.y || abs_float @@ now () -. t > 0.5
        || (state.layout != [] && layoutready state.layout)
     then (
       state.reload <- None;
       display ()
     )
  | None -> display ()
;;

let zoomrect x y x1 y1 =
  let x0 = min x x1
  and x1 = max x x1
  and y0 = min y y1 in
  let zoom = (float state.w) /. float (x1 - x0) in
  let margin =
    let simple () =
      if state.w < state.winw
      then (state.winw - state.w) / 2
      else 0
    in
    match conf.fitmodel with
    | FitWidth | FitProportional -> simple ()
    | FitPage ->
       match conf.columns with
       | Csplit _ ->
          onppundermouse (fun _ l _ _ -> Some l.pagedispx) x0 y0 x0
       | Cmulti _ | Csingle _ -> simple ()
  in
  gotoxy ((state.x + margin) - x0) (state.y + y0);
  state.anchor <- getanchor ();
  setzoom zoom;
  resetmstate ();
;;

let annot inline x y =
  match unproject x y with
  | Some (opaque, n, ux, uy) ->
     let add text =
       Ffi.addannot opaque ux uy text;
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
       postRedisplay "annot"
     else add @@ getusertext E.s
  | _ -> ()
;;

let zoomblock x y =
  let g opaque l px py =
    match Ffi.rectofblock opaque px py with
    | Some a ->
       let x0 = a.(0) -. 20. in
       let x1 = a.(1) +. 20. in
       let y0 = a.(2) -. 20. in
       let zoom = (float state.w) /. (x1 -. x0) in
       let pagey = getpagey l.pageno in
       let margin = (state.w - l.pagew)/2 in
       let nx = -truncate x0 - margin in
       gotoxy nx (pagey + truncate y0);
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
  let destx = truncate (float (state.w + winw) *. s) in
  gotoxy (winw - destx) state.y;
  state.mstate <- Mscrollx;
;;

let scrolly y =
  let s = float y /. float state.winh in
  let desty = truncate (s *. float (maxy ())) in
  gotoxy state.x desty;
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
    if Ffi.markunder opaque px py mark
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
  postRedisplay "viewmulticlick";
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
       let incr =
         if n = 5
         then if conf.zoom +. 0.01 > 0.1 then 0.1 else 0.01
         else if conf.zoom -. 0.1 < 0.1 then -0.01 else -0.1
       in
       let fx, fy =
         match state.mstate with
         | Mzoom (oldn, _, pos) when n = oldn -> pos
         | Mzoomrect _ | Mnone | Mpan _
         | Msel _ | Mscrollx | Mscrolly | Mzoom _ -> (x, y)
       in
       let zoom = conf.zoom -. incr in
       state.mstate <- Mzoom (n, 0, (x, y));
       if false && abs (fx - x) > 5 || abs (fy - y) > 5
       then pivotzoom ~x ~y zoom
       else pivotzoom zoom
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
            let incr = if n = 4 then -conf.scrollstep else conf.scrollstep in
            let incr = incr * 2 in
            let y = clamp incr in
            gotoxy state.x y
     )

  | n when (n = 6 || n = 7) && not down && canpan () ->
     let x =
       panbound (state.x + (if n = 7 then -2 else 2) * conf.hscrollstep) in
     gotoxy x state.y

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
          | exception exn ->
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
     else state.mstate <- Mnone

  | 3 ->
     if down
     then (
       if Wsi.withshift mask
       then (
         annot conf.annotinline x y;
         postRedisplay "addannot"
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
            postRedisplay "kill accidental zoom rect";
          )
       | Msel _
       | Mpan _
       | Mscrolly | Mscrollx
       | Mzoom _
       | Mnone -> resetmstate ()
     )

  | 1 when vscrollhit x ->
     if down
     then
       let _, position, sh = state.uioh#scrollph in
       if y > truncate position && y < truncate (position +. sh)
       then state.mstate <- Mscrolly
       else scrolly y
     else state.mstate <- Mnone

  | 1 when y > state.winh - hscrollh () ->
     if down
     then
       let _, position, sw = state.uioh#scrollpw in
       if x > truncate position && x < truncate (position +. sw)
       then state.mstate <- Mscrollx
       else scrollx x
     else state.mstate <- Mnone

  | 1 when state.bzoom -> if not down then zoomblock x y

  | 1 ->
     let dest = if down then getunder x y else Unone in
     begin match dest with
     | Ulinkuri _ -> gotounder dest
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
            postRedisplay "mouse select";
          )
        )
        else (
          match state.mstate with
          | Mnone -> ()
          | Mzoom _ | Mscrollx | Mscrolly -> state.mstate <- Mnone
          | Mzoomrect ((x0, y0), _) -> zoomrect x0 y0 x y
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
                         pipef ~closew:false "Msel"
                           (fun w ->
                             Ffi.copysel w opaque;
                             postRedisplay "Msel") cmd
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
          then
            leavebirdseye (conf, leftx, l.pageno, hooverpageno, anchor) false
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
      | LinkNav _ | View -> viewmouse button bstate x y mask
      | Birdseye beye -> birdseyemouse button bstate x y mask beye
      | Textentry _ -> ()
      end;
      state.uioh

    method multiclick clicks x y mask =
      begin match state.mode with
      | LinkNav _ | View -> viewmulticlick clicks x y mask
      | Birdseye _ | Textentry _ -> ()
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
            let x = if canpan () then panbound (state.x + dx) else state.x in
            let y = clamp dy in
            gotoxy x y

         | Msel (a, _) ->
            state.mstate <- Msel (a, (x, y));
            postRedisplay "motion select";

         | Mscrolly ->
            let y = min state.winh (max 0 y) in
            scrolly y

         | Mscrollx ->
            let x = min state.winw (max 0 x) in
            scrollx x

         | Mzoomrect (p0, _) ->
            state.mstate <- Mzoomrect (p0, (x, y));
            postRedisplay "motion zoomrect";
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
                postRedisplay "pmotion birdseye no hoover";
              )
           | l :: rest ->
              if y > l.pagedispy && y < l.pagedispy + l.pagevh
                 && x > l.pagedispx && x < l.pagedispx + l.pagevw
              then (
                state.mode <- Birdseye (conf, leftx, pageno, l.pageno, anchor);
                postRedisplay "pmotion birdseye hoover";
              )
              else loop rest
         in
         loop state.layout

      | Textentry _ -> ()

      | LinkNav _ | View ->
         match state.mstate with
         | Mpan _ | Msel _ | Mzoom _ | Mscrolly | Mscrollx | Mzoomrect _ -> ()
         | Mnone ->
            updateunder x y;
            if canselect ()
            then
              match conf.pax with
              | None -> ()
              | Some past ->
                 let now = now () in
                 let delta = now -. past in
                 if delta > 0.01
                 then paxunder x y
                 else conf.pax <- Some now
      end;
      state.uioh

    method infochanged _ = ()

    method scrollph =
      let maxy = maxy () in
      let p, h =
        if maxy = 0
        then 0.0, float state.winh
        else scrollph state.y maxy
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
        let x = state.winw - state.x in
        let percent = float x /. float maxx in
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
    method scroll dx dy =
      let x = if canpan () then panbound (state.x + dx) else state.x in
      gotoxy x (clamp (2 * dy));
      state.uioh
    method zoom z x y =
      pivotzoom ~x ~y (conf.zoom *. exp z);
  end;;

let addrect pageno r g b a x0 y0 x1 y1 =
  Hashtbl.add state.prects pageno [|r; g; b; a; x0; y0; x1; y1|];
;;

let ract cmds =
  let cl = splitatchar cmds ' ' in
  let scan s fmt f =
    try Scanf.sscanf s fmt f
    with exn -> adderrfmt "remote exec" "error processing '%S': %s\n"
                  cmds @@ exntos exn
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
        postRedisplay s;
      )
  in
  match cl with
  | "reload", "" -> reload ()
  | "goto", args ->
     scan args "%u %f %f"
       (fun pageno x y ->
         let cmd, _ = state.geomcmds in
         if emptystr cmd
         then gotopagexy pageno x y
         else
           let f prevf () =
             gotopagexy pageno x y;
             prevf ()
           in
           state.reprf <- f state.reprf
       )
  | "goto1", args -> scan args "%u %f" gotopage
  | "gotor", args -> scan args "%S" gotoremote
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
         postRedisplay "prect"
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
              then gotoxy (state.x - l.pagedispx) state.y
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
       with exn -> adderrfmt "error processing keys" "`%S': %s\n"
                     cmds @@ exntos exn
     end
  | "clearrects", "" ->
     Hashtbl.clear state.prects;
     postRedisplay "clearrects"
  | _ ->
     adderrfmt "remote command"
       "error processing remote command: %S\n" cmds;
;;

let remote =
  let scratch = Bytes.create 80 in
  let buf = Buffer.create 80 in
  fun fd ->
  match tempfailureretry (Unix.read fd scratch 0) 80 with
  | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> None
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
         | exception Not_found -> -1
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
  Utils.vlogf := (fun s -> if conf.verbose then prerr_endline s else ignore s);
  let gcconfig = ref false in
  let rcmdpath = ref E.s in
  let pageno = ref None in
  let openlast = ref false in
  let doreap = ref false in
  let csspath = ref None in
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

        ("-tcf", Arg.String (fun s -> Config.tcfpath := s),
         "<path> Set path to the trim cache file");

        ("-dest", Arg.String (fun s -> state.nameddest <- s),
         "<named-destination> Set named destination");

        ("-remote", Arg.String (fun s -> rcmdpath := s),
         "<path> Set path to the source of remote commands");

        ("-gc", Arg.Set gcconfig, " Collect config garbage");

        ("-v", Arg.Unit (fun () ->
                   Printf.printf
                     "%s\nconfiguration file: %s\n"
                     (Help.version ())
                     Config.defconfpath;
                   exit 0), " Print version and exit");

        ("-css", Arg.String (fun s -> csspath := Some s),
         "<path> Set path to the style sheet to use with EPUB/HTML");

        ("-origin", Arg.String (fun s -> state.origin <- s),
         "<origin> <undocumented>");

        ("-no-title", Arg.Set ignoredoctitlte, " ignore document title");
        ("-layout-height", Arg.Set_int layouth,
         "<height> layout height html/epub/etc (-1, 0, N)");
       ]
    )
    (fun s -> state.path <- s)
    ("Usage: " ^ Sys.argv.(0) ^ " [options] some.pdf\nOptions:");

  let histmode = emptystr state.path && not !openlast in

  if not (Config.load !openlast)
  then dolog "failed to load configuration";

  begin match !pageno with
  | Some pageno -> state.anchor <- (pageno, 0.0, 0.0)
  | None -> ()
  end;

  fillhelp ();
  if !gcconfig
  then (
    Config.gc ();
    exit 0
  );

  let mu =
    object (self)
      val mutable m_clicks = 0
      val mutable m_click_x = 0
      val mutable m_click_y = 0
      val mutable m_lastclicktime = infinity

      method private cleanup =
        state.roam <- noroam;
        Hashtbl.iter (fun _ opaque -> Ffi.clearmark opaque) state.pagemap
      method expose = postRedisplay "expose"
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
          (*
           * http://blogs.msdn.com/b/oldnewthing/archive/2004/10/18/243925.aspx
           *)
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
              postRedisplay "cleanup";
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
        else state.uioh <- state.uioh#button b d x y m
      method motion x y =
        state.mpos <- (x, y);
        state.uioh <- state.uioh#motion x y
      method pmotion x y =
        state.mpos <- (x, y);
        state.uioh <- state.uioh#pmotion x y
      method key k m =
        vlog "k=%#x m=%#x" k m;
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
      method quit : 'a. 'a = raise Quit
      method scroll dx dy = state.uioh <- state.uioh#scroll dx dy
      method zoom z x y = state.uioh#zoom z x y
      method opendoc path =
        state.mode <- View;
        state.uioh <- uioh;
        postRedisplay "opendoc";
        opendoc path state.password
    end
  in
  if !Config.tcfpath == E.s
  then Config.tcfpath := conf.trimcachepath;
  let wsfd, winw, winh = Wsi.init mu conf.cwinw conf.cwinh platform in
  state.wsfd <- wsfd;

  if not @@ List.exists GlMisc.check_extension
              [ "GL_ARB_texture_rectangle"
              ; "GL_EXT_texture_recangle"
              ; "GL_NV_texture_rectangle" ]
  then (dolog "OpenGL does not suppport rectangular textures"; exit 1);

  let cs, ss =
    match Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 with
    | exception exn ->
       dolog "socketpair failed: %s" @@ exntos exn;
       exit 1
    | (r, w) ->
       cloexec r;
       cloexec w;
       r, w
  in

  setcheckers conf.checkers;
  begin match !csspath with
  | None -> ()
  | Some "" -> conf.css <- E.s
  | Some path ->
     let css = filecontents path in
     let l = String.length css in
     conf.css <-
       if substratis css (l-2) "\r\n"
       then String.sub css 0 (l-2)
       else (if css.[l-1] = '\n' then String.sub css 0 (l-1) else css)
  end;
  Ffi.settrimcachepath !Config.tcfpath;
  conf.trimcachepath <- !Config.tcfpath;
  Ffi.init cs (
      conf.angle, conf.fitmodel, (conf.trimmargins, conf.trimfuzz),
      conf.texcount, conf.sliceheight, conf.mustoresize,
      conf.colorspace, !Config.fontpath
    );
  List.iter GlArray.enable [`texture_coord; `vertex];
  GlTex.env (`color conf.texturecolor);
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
    | exception (Unix.Unix_error (Unix.ECHILD, _, _)) -> ()
    | exception exn -> dolog "Unix.waitpid: %s" @@ exntos exn
    | 0, _ -> ()
    | _pid, _status -> reap ()
  in
  Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> doreap := true));

  let optrfd =
    ref (if nonemptystr !rcmdpath then remoteopen !rcmdpath else None)
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
    if !redisplay
    then (
      Glutils.redisplay := false;
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
       let newdeadline =
         match state.autoscroll with
         | Some step when step != 0 ->
            if state.slideshow land 1 = 1
            then (
              if state.slideshow land 2 = 0
              then state.slideshow <- state.slideshow lor 2
              else if step < 0 then prevpage () else nextpage ();
              deadline +. (float (abs step))
            )
            else
              let y = state.y + step in
              let fy = if conf.maxhfit then state.winh else 0 in
              let y =
                if y < 0
                then state.maxy - fy
                else if y >= state.maxy - fy then 0 else y
              in
              gotoxy state.x y;
              deadline +. 0.01
         | _ -> infinity
       in
       loop newdeadline

    | l ->
       let rec checkfds = function
         | [] -> ()
         | fd :: rest when fd = state.ss ->
            let cmd = Ffi.rcmd state.ss in
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
         | _ -> infinity
       in
       loop newdeadline
    end;
  in
  match loop infinity with
  | exception Quit ->
     Config.save leavebirdseye;
     if Ffi.hasunsavedchanges ()
     then save ()
  | _ -> error "umpossible - infinity reached"
;;
