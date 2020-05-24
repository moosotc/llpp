open Utils;;
open Glutils;;
open Config;;

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

let isbirdseye = function
  | Birdseye _ -> true
  | Textentry _ | View | LinkNav _ -> false
;;

let istextentry = function
  | Textentry _ -> true
  | Birdseye _ | View | LinkNav _ -> false
;;

let vscrollw () =
  if state.uioh#alwaysscrolly || ((conf.scrollb land scrollbvv != 0)
                                  && (state.maxy > state.winh))
  then conf.scrollbw
  else 0
;;

let vscrollhit x =
  if conf.leftscroll
  then x < vscrollw ()
  else x > state.winw - vscrollw ()
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
      filledrect
        x      (float (state.winh - (fstate.fontsize + 4) - hscrollh))
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
    drawstring
      fstate.fontsize
      (if conf.leftscroll then x0 + 2 else x0 + if len > 0 then 8 else 2)
      (state.winh - hscrollh - 5) s;
  in
  let s =
    match state.mode with
    | Textentry ((prefix, text, _, _, _, _), _) ->
       let s =
         if len > 0
         then Printf.sprintf "%s%s_ [%s]" prefix text state.text
         else Printf.sprintf "%s%s_"  prefix text
       in
       s

    | Birdseye _ | View | LinkNav _ -> state.text
  in
  let s =
    if state.newerrmsgs
    then (
      if not (istextentry state.mode) && state.uioh#eformsgs
      then
        let s1 = "(press 'e' to review error messages)" in
        if nonemptystr s then s ^ " " ^ s1 else s1
      else s
    )
    else s
  in
  if nonemptystr s
  then drawstring s
;;

let textentrykeyboard
      key mask ((c, text, opthist, onkey, ondone, cancelonempty), onleave) =
  state.text <- E.s;
  let enttext te =
    state.mode <- Textentry (te, onleave);
    enttext ();
    postRedisplay "textentrykeyboard enttext";
  in
  let histaction cmd =
    match opthist with
    | None -> ()
    | Some (action, _) ->
       state.mode <- Textentry (
                         (c, action cmd, opthist,
                          onkey, ondone, cancelonempty),
                         onleave);
       postRedisplay "textentry histaction"
  in
  let open Keys in
  let kt = Wsi.kc2kt key in
  match [@warning "-4"] kt with
  | Backspace ->
     if emptystr text && cancelonempty
     then (
       onleave Cancel;
       postRedisplay "textentrykeyboard after cancel";
     )
     else
       let s = withoutlastutf8 text in
       enttext (c, s, opthist, onkey, ondone, cancelonempty)

  | Enter ->
     ondone text;
     onleave Confirm;
     postRedisplay "textentrykeyboard after confirm"

  | Up   -> histaction HCprev
  | Down -> histaction HCnext
  | Home -> histaction HCfirst
  | End  -> histaction HClast

  | Escape ->
     if emptystr text
     then (
       begin match opthist with
       | None -> ()
       | Some (_, onhistcancel) -> onhistcancel ()
       end;
       onleave Cancel;
       state.text <- E.s;
       postRedisplay "textentrykeyboard after cancel2"
     )
     else enttext (c, E.s, opthist, onkey, ondone, cancelonempty)

  | Delete -> ()

  | Insert when Wsi.withshift mask ->
     let s = getcmdoutput (fun s ->
                 prerr_endline ("error pasting: " ^ s)) conf.pastecmd in
     enttext (c, s, opthist, onkey, ondone, cancelonempty)

  | Code _ | Ascii _ ->
     begin match onkey text kt with
     | TEdone text ->
        ondone text;
        onleave Confirm;
        postRedisplay "textentrykeyboard after confirm2";

     | TEcont text ->
        enttext (c, text, opthist, onkey, ondone, cancelonempty);

     | TEstop ->
        onleave Cancel;
        postRedisplay "textentrykeyboard after cancel3";

     | TEswitch te ->
        state.mode <- Textentry (te, onleave);
        postRedisplay "textentrykeyboard switch";
     end
  | _ -> vlog "unhandled key"
;;

class type lvsource =
  object
    method getitemcount : int
    method getitem : int -> (string * int)
    method hasaction : int -> bool
    method exit : uioh:uioh ->
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

class virtual lvsourcebase =
        object
          val mutable m_active = 0
          val mutable m_first = 0
          val mutable m_pan = 0
          method getactive = m_active
          method getfirst = m_first
          method getpan = m_pan
          method getminfo : (int * int) array = E.a
        end;;

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
      if not ((row - m_first) > fstate.maxrows)
      then (
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
              let s1, s2 = splitatchar s '\000' in
              if emptystr s2
              then drawstring1 fs x' (y+nfs) s
              else
                let rec e s =
                  if emptystr s
                  then s
                  else
                    let s' = withoutlastutf8 s in
                    let s = s' ^ Utf8syms.ellipsis in
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
              let s1, s2 = splitatchar s '\t' in
              if nonemptystr s2
              then
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
                else drawstr x s
            else drawstr x s
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
      if (row - m_first) <= fstate.maxrows
      then
        if row >= 0 && row < itemcount
        then
          let (s, level) = source#getitem row in
          let pos0 = Ne.index s '\000' in
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
    in
    Gl.disable `texture_2d;
    if Array.length minfo > 0 then loop m_first;
    Gl.disable `blend;
    if conf.leftscroll
    then GlMat.pop ()

  method nextcurlevel incr =
    let len = source#getitemcount in
    let curlevel =
      if m_active >= 0 && m_active < len
      then snd (source#getitem m_active)
      else -1
    in
    let rec flow i =
      if i = len
      then i-1
      else (
        if i = -1
        then 0
        else
          let _, l = source#getitem i in
          if l <= curlevel then i else flow (i+incr)
      )
    in
    let active = flow (m_active+incr) in
    let first = calcfirst m_first active in
    postRedisplay "outline nextcurlevel";
    {< m_active = active; m_first = first >}

  method updownlevel incr =
    let len = source#getitemcount in
    let curlevel =
      if m_active >= 0 && m_active < len
      then snd (source#getitem m_active)
      else -1
    in
    let rec flow i =
      if i = len
      then i-1
      else (
        if i = -1 then 0 else
          let _, l = source#getitem i in
          if l != curlevel then i else flow (i+incr)
      )
    in
    let active = flow m_active in
    let first = calcfirst m_first active in
    postRedisplay "outline updownlevel";
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
            | exception Not_found -> loop (n + incr)
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
         dolog "regexp_case_fold for `%S' failed: %S\n" qpat @@
           Printexc.to_string exn;
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
      postRedisplay "listview navigate";
      set active first;
    in
    let open Keys in
    let kt = Wsi.kc2kt key in
    match [@warning "-4"] kt with
    | Ascii (('r'|'s') as c) when Wsi.withctrl mask ->
       let incr = if c = 'r' then -1 else 1 in
       let active, first =
         match search (m_active + incr) m_qsearch incr with
         | None ->
            state.text <- m_qsearch ^ " [not found]";
            m_active, m_first
         | Some active ->
            state.text <- m_qsearch;
            active, firstof m_first active
       in
       postRedisplay "listview ctrl-r/s";
       set1 active first m_qsearch;

    | Insert when Wsi.withctrl mask ->
       if m_active >= 0 && m_active < source#getitemcount
       then (
         let s, _ = source#getitem m_active in
         selstring conf.selcmd s;
       );
       coe self

    | Backspace ->
       if emptystr m_qsearch
       then coe self
       else (
         let qsearch = withoutlastutf8 m_qsearch in
         if emptystr qsearch
         then (
           state.text <- E.s;
           postRedisplay "listview empty qsearch";
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
           postRedisplay "listview backspace qsearch";
           set1 active first qsearch
       );

    | Ascii _ | Code _ ->
       let utf8 =
         match [@warning "-8"] kt with
         | Ascii c -> String.make 1 c
         | Code code -> toutf8 code
       in
       let pattern = m_qsearch ^ utf8 in
       let active, first =
         match search m_active pattern 1 with
         | None ->
            state.text <- pattern ^ " [not found]";
            m_active, m_first
         | Some active ->
            state.text <- pattern;
            active, firstof m_first active
       in
       postRedisplay "listview qsearch add";
       set1 active first pattern;

    | Escape ->
       state.text <- E.s;
       if emptystr m_qsearch
       then (
         postRedisplay "list view escape";
         (* XXX:
             let mx, my = state.mpos in
             updateunder mx my;
          *)
         getoptdef m_prev_uioh @@
           source#exit ~uioh:(coe self) ~cancel:true ~active:m_active
             ~first:m_first ~pan:m_pan
       )
       else (
         postRedisplay "list view kill qsearch";
         coe {< m_qsearch = E.s >}
       )

    | Enter ->
       state.text <- E.s;
       let self = {< m_qsearch = E.s >} in
       let opt =
         postRedisplay "listview enter";
         let cancel = not (m_active >= 0 && m_active < source#getitemcount) in
         source#exit ~uioh:(coe self) ~cancel
                     ~active:m_active ~first:m_first ~pan:m_pan;
       in
       getoptdef m_prev_uioh opt

    | Delete -> coe self
    | Up    -> navigate ~-1
    | Down  -> navigate 1
    | Prior -> navigate ~-(fstate.maxrows)
    | Next  -> navigate fstate.maxrows

    | Right ->
       state.text <- E.s;
       postRedisplay "listview right";
       coe {< m_pan = m_pan - 1 >}

    | Left ->
       state.text <- E.s;
       postRedisplay "listview left";
       coe {< m_pan = m_pan + 1 >}

    | Home ->
       let active = find 0 1 in
       postRedisplay "listview home";
       set active 0;

    | End ->
       let first = max 0 (itemcount - fstate.maxrows) in
       let active = find (itemcount - 1) ~-1 in
       postRedisplay "listview end";
       set active first;

    | _ -> coe self

  method key key mask =
    match state.mode with
    | Textentry te ->
       textentrykeyboard key mask te;
       coe self
    | Birdseye _ | View | LinkNav _ -> self#key1 key mask

  method button button down x y _ =
    let opt =
      match button with
      | 1 when vscrollhit x ->
         postRedisplay "listview scroll";
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
            postRedisplay "listview click";
            source#exit ~uioh:(coe {< m_active = n >})
                        ~cancel:false ~active:n ~first:m_first ~pan:m_pan
         | _ ->
            Some (coe self)
         end
      | n when (n == 4 || n == 5) && not down ->
         let len = source#getitemcount in
         let first =
           if n = 5 && m_first + fstate.maxrows >= len
           then m_first
           else
             let first = m_first + (if n == 4 then -1 else 1) in
             bound first 0 (len - 1)
         in
         postRedisplay "listview wheel";
         Some (coe {< m_first = first >})
      | n when (n = 6 || n = 7) && not down ->
         let inc = if n = 7 then -1 else 1 in
         postRedisplay "listview hwheel";
         Some (coe {< m_pan = m_pan + inc >})
      | _ -> Some (coe self)
    in
    getoptdef m_prev_uioh opt

  method multiclick _ x y = self#button 1 true x y

  method motion _ y =
    match state.mstate with
    | Mscrolly ->
       let s = float (max 0 (y - conf.scrollh)) /. float state.winh in
       let first = truncate (s *. float source#getitemcount) in
       let first = min source#getitemcount first in
       postRedisplay "listview motion";
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
        then (postRedisplay "listview pmotion"; {< m_active = n >})
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
  method scroll _ dy =
    let self =
      if dy != 0 then begin
        let len = source#getitemcount in
        let first =
          if dy > 0 && m_first + fstate.maxrows >= len
          then m_first
          else
            let first = m_first + dy / 10 in
            bound first 0 (len - 1)
        in
        postRedisplay "listview wheel";
        {< m_first = first >}
      end else
        self
    in
    coe self

  method zoom _ _ _ = ()
end;;
