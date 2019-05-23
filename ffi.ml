open Config;;

type initparams = (angle * fitmodel * trimparams * texcount * sliceheight *
                     memsize * colorspace * fontpath * trimcachepath);;

external init : Unix.file_descr -> initparams -> unit = "ml_init";;
external seltext : opaque -> (int * int * int * int) -> unit = "ml_seltext";;
external hassel : opaque -> bool = "ml_hassel";;
external getpdimrect : int -> float array = "ml_getpdimrect";;
external whatsunder : opaque -> x -> y -> under = "ml_whatsunder";;
external markunder : opaque -> x -> y -> mark -> bool = "ml_markunder";;
external clearmark : opaque -> unit = "ml_clearmark";;
external zoomforh : int -> int -> int -> int -> float = "ml_zoom_for_height";;
external getmaxw : unit -> float = "ml_getmaxw";;
external postprocess :
  opaque -> int -> int -> int -> (int * string * int) -> int
  = "ml_postprocess";;
external pagebbox : opaque -> bbox = "ml_getpagebox";;
external setaalevel : int -> unit = "ml_setaalevel";;
external setpapercolor : (float * float * float * float) -> unit = "ml_setpapercolor";;
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
external drawprect : opaque -> int -> int -> float array -> unit
  = "ml_drawprect";;
external wcmd : Unix.file_descr -> bytes -> int -> unit = "ml_wcmd";;
external rcmd : Unix.file_descr -> string = "ml_rcmd";;
external uritolocation : string -> (pageno * float * float)
  = "ml_uritolocation";;
external isexternallink : string -> bool = "ml_isexternallink";;

(* copysel _will_ close the supplied descriptor *)
external copysel : Unix.file_descr -> opaque -> unit = "ml_copysel";;

external drawstr : int -> int -> int -> string -> float = "ml_draw_string";;

external fz_version : unit -> string = "ml_fz_version";;
external llpp_version : unit -> string = "ml_llpp_version";;
