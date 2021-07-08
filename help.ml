let keys =
{|-----Quitting-----
escape/q  - quit
Q         - quit without saving the configuration or changes
W         - save changes

-----Movement-----
up/down arrow        - scroll up/down
left/right arrow     - pan left/right (when zoomed in)
Ctrl-arrows          - half a screen width/height scoll/pan
space                - go to the next page
Shift-space, delete  - go to the previous page
pageup/pagedown      - go forward/backward one screen-full
g/G                  - go to first/last page
home/end             - go to first/last page

-----Advanced Movement / History-----
backspace             - go back after jumping (clicking link and suchlike)[1]
Alt-left/right arrow  - go backward/forward in history
0..9                  - enter page number to jump to
t                     - align top of the screen with the top of the page
<,>                   - rotate
Ctrl-pageup/pagedown  - align top of the screen with first/last visible page
F                     - go to hinted link
if auto scrolling is active:
    up/down arrows and mouse buttons 3/4 will make scrolling go faster/slower

-----Mouse-----
primary mouse button    - click on link or select text[2]
  when Shift is held    - run "synctex command" with coordinates of the point[3]
  when Ctrl is held     - pan
  zoom to block         - when in block zoom mode
  otherwise             - pan if there's no text/link under the cursor
mouse buttons 3/4       - scroll up/down (aka mouse wheel)
  when Shift is held    - pan left/right
  when Ctrl is held     - zoom
mouse buttons 5/6       - pan left/right (aka horizontal mouse wheel)
secondary mouse button  - select rectangle to zoom to
  when Shift is held    - add text annotation

Note:
  moving mouse while holding primary mouse button will pan the page,
  if the mouse is over selectable element (text/link) holding Ctrl
  will make the page pan instead of the default action (selecting text/
  clicking link)

-----Bookmarks-----
m                   - create named bookmark
~                   - create quick bookmark

-----Zoom-----
Ctrl+Shift up/down  - set previous zoom level
B                   - toggle zoom block mode
w                   - change height of the window to encompass all of the page
Alt-c               - center view
Ctrl-+/Ctrl-=       - zoom in
Ctrl--              - zoom out
Ctrl-0              - reset zoom and panning
Ctrl-1              - fit tallest page
Ctrl-2              - fit tallest page height exactly
Ctrl-3              - cycle fit models
Ctrl-4              - 1:1 zoom
Ctrl-9/F9           - enter bird's eye view

-----Actions-----
u                   - dehighlight
r                   - reload document[4]
y                   - select link and paste its description to the clipboard
|                   - pipe selection through specified command
x                   - run pax command on selected region
Ctrl-p              - launch a command with the document path as an argument
return              - (in link navigation mode) follow link
=                   - show current position
e                   - view error log

-----Search-----
/,?                 - enter text to search for (/ - forward, ? - backward)
n                   - repeat last search (forward)
p, N                - repeat last search (backward)
z[zbt]              - "zoom in" on the first search result
Ctrl-g              - interrupt search

-----Settings / Modes-----
[/]                 - decrease/increase page brightness
+                   - set page bias
-                   - toggle/set tunable
b                   - toggle scroll bar
l                   - toggle highlighting of the links
f                   - toggle fullscreen
insert              - toggle link navigation mode
o                   - switch to outline/table of contents mode
'                   - switch to bookmark mode
F1                  - switch to help mode
Alt-F1              - switch to history mode
i                   - switch to info mode
P                   - switch to "presentation" mode
c                   - switch to previous column layout
a                   - auto scroll mode

-----Tunables-----
-i          - toggle case sensitivity of searches
-R<number>  - rotate
-v          - toggle verbosity
-Z<number>  - set zoom (percent)
-T          - toggle trimming of margins
-I          - invert colors
-M          - toggle pax mode
-C<number>  - set column count
-x<string>  - set command to run on selection
-f          - toggle "what's under cursor" identification
            - (or "what's currently selected" in link navigation mode)
            -     (font name of the text under cursor or link target)

-----Bird's eye mode-----
Ctrl-9/F9/esc         - leave bird's eye view
Ctrl-l                - center the view on the currently selected page
(page)up/(page)down   - navigate
home/end              - go to first/last page
enter                 - select the page and leave bird's eye mode
primary mouse button  - select the page under cursor and leave bird's eye mode

-----Outline/bookmark/help/info/history mode-----
esc              - leave outline/bookmark mode
up/down          - select previous/next item
alpha-numeric    - quick search
Ctrl-s           - repeat search (forward)
Ctrl-r           - repeat search (backward)
Ctrl-l           - center on current item
Ctrl-left/right  - pan text left/right
Ctrl-up/down     - scroll view up/down
Ctrl-insert      - copy active item's text to clipboard

-----Outline/history mode-----
left/right       - go up/down a level
Shift-left/right - go to prev/next item of same or higher level
Ctrl-a           - toggle auto narrowing
/                - enter auto narrowing
tab              - begin nested narrowing
Ctrl-n           - narrow to search pattern
Ctrl-u           - undo narrowing
Ctrl-S           - synchronize

-----Info mode-----
enter - toggle/enter new value for selected parameter

-----When entering search pattern/page number-----
up/down arrow  - previous/next entered text
enter          - confirm
Ctrl-g/esc     - cancel

double/triple/quadruple/quintuple clicks in view mode will pipe the
word/line/block/page through pax command when Ctrl is held and
selection command otherwise

[1] if the document was previously visited initial backspace will
    jump to the last visited place

[2] C&P

[3] arguments to the command are:
    1 = path to the document
    2 = zero based page number
    3 = X coordinate within the page
    4 = Y …

[4] document will also be reloaded upon reception of the HUP signal

-----Caveat emptor-----
o Text selection is limited to a single page
o Text searching is very naive|}

open Utils

let gotourl launcher url =
  let command = Str.global_replace Utils.Re.percent url launcher in
  try ignore @@ spawn command []
  with exn -> dolog "failed to execute `%s': %s" command @@ exntos exn

let gotouri launcher uri =
  if emptystr launcher
  then dolog "%s" uri
  else
    if nonemptystr @@ geturl uri
    then gotourl launcher uri
    else dolog "obtained empty url from uri %S" uri

let version () =
  Printf.sprintf "llpp %s, ocaml %s (%d bit), fitz %s"
    (Ffi.llpp_version ()) Sys.ocaml_version Sys.word_size (Ffi.fz_version ())

let fixup =
  let gr = Str.global_replace in
  let dash = Str.regexp {|\([^ ]*\) +- +\(.*\)|}
  and head = Str.regexp {|-----\(.*\)-----|}
  and wcAp = Str.regexp "C&P" in
  fun s -> gr dash "\\1\t\\2" @@ gr head "\xc2\xb7\\1" @@ gr wcAp Wsi.cAp s

let makehelp launcher =
  version ()
  :: "(searching in this text works just by typing (i.e. no initial '/'))"
  :: E.s :: String.split_on_char '\n' keys |>
    List.map (fun s ->
        let s = fixup s in
        match geturl s with
        | "" -> (s, 0, None)
        | url ->  (s, 0, Some (fun uioh -> gotourl launcher url; uioh))
      )
