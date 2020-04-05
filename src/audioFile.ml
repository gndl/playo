(* 
 * Copyright (C) 2015 Gaëtan Dubreil
 *
 *  All rights reserved.This file is distributed under the terms of the
 *  GNU General Public License version 3.0.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Unix
open Usual
open FFmpeg
open Avutil

module InRsp = Swresample.Make (Swresample.Frame) (Swresample.FltBigArray)


let audio_ext = [ ".wav"; ".ogg"; ".flac"; ".mp3"; ".pls"](*; ".m3u"*)
let audio_ext_pattern = L.map ~f:(fun ext -> "*"^ext) audio_ext

let outRate = 44100
let outChannelLayout = `Stereo
let outSampleFormat = Swresample.FltBigArray.sf

let uk = ""
let defRate = Int64.of_int outRate

type state = Single | Repeat | Track | Random | Pause | Off

type talker = {stream : (input, audio)Av.stream; input_resampler : InRsp.t}


type t = {
  mutable title : string;
  mutable artist : string;
  mutable album : string;
  mutable genre : string;
  fnode : node;
  mutable duration : Int64.t;
  mutable curPosition : Int64.t;
  mutable newPosition : Int64.t;
  mutable readPercent : int;
  mutable rate : Int64.t;
  mutable channels : int;
  mutable container : input container option;
  mutable voice : talker option
}
and node = {
  name : string;
  path : string;
  mutable time : string;
  mutable size : string;
  mutable kind : kind;
  mutable idx : int;
  mutable parent : dir option;
  mutable state : state;
  mutable visible : bool
}
and dir = {
  dnode : node;
  mutable children : node array
}
and kind = File of t | Dir of dir | Null

let title file = file.title
let artist file = file.artist
let album file = file.album
let genre file = file.genre
let node file = file.fnode
let duration file = file.duration
let currentPosition file = file.curPosition
let newPosition file = file.newPosition
let readPercent file = file.readPercent
let rate file = Int64.to_int file.rate
let channels file = file.channels
let container file = file.container
let voice file = file.voice
let name file = file.fnode.name
let path file = file.fnode.path
let filename file = file.fnode.path^file.fnode.name
let time file = file.fnode.time
let kind file = file.fnode.kind
let idx file = file.fnode.idx
let parent file = file.fnode.parent
let state file = file.fnode.state

let makeNode ?(idx = 0) ?(kind = Null) ?(parent = None) ?(state = Off)
    ?(visible = true) ?(time = uk) ?(size = uk) name path =
  {name; path; time; size; kind; idx; parent; state; visible}


let makeDir ?(path = "") ?(idx = 0) ?(children = [||]) ?(parent = None) ?(state = Off) ?(time = uk) name =
  let dnode = {name; path; time; size = soi(A.length children);
	       kind = Null; idx; parent; state; visible = true}
  in
  let dir = {dnode; children} in dnode.kind <- Dir dir; dir


let makeFile ?(idx = 0) ?(parent = None) ?(state = Off) ?(time = uk)
    ?(title = uk) ?(artist = uk) ?(album = uk) ?(genre = uk) ?(voice = None)
    ?(rate = Int64.zero) ?(channels = 1) ?(container = None) ?(size = uk) name path =

  let fnode = {name; path; time; size; kind = Null; idx; parent; state; visible = true}
  in
  let file = {title; artist; album; genre; fnode;
              duration = Int64.zero; curPosition = Int64.zero; newPosition = Int64.zero;
              readPercent = 0; rate;
              channels; container; voice}
  in
  fnode.kind <- File file; file



let unexistentNode = {name = uk; path = uk; time = uk; size = uk; kind = Null; idx = -1; parent = None; state = Off; visible = false}
let unexistentFile = {title = uk; artist = uk; album = uk; genre = uk;
                      fnode = unexistentNode;
                      duration = Int64.zero;
                      curPosition = Int64.zero; newPosition = Int64.zero;
                      readPercent = 0; rate = defRate; channels = 1;
                      container = None; voice = None}
let unexistentDir = {dnode = unexistentNode; children = [||]}


let hasInfo f = f.title <> uk || f.artist <> uk || f.album <> uk || f.genre <> uk
let hasId f = f.title <> uk || f.artist <> uk || f.album <> uk

let secondFractions = Int64.of_int 1_000_000_000
let hundred = Int64.of_int 100

let secondsToTime seconds =
  let h = seconds / 3600 in
  let m = seconds / 60 - h * 60 in
  let s = seconds - m * 60 - h * 3600 in
  if h > 0 then Printf.sprintf " %d:%02d:%02d " h m s
  else Printf.sprintf " %d:%02d " m s

let samplesToTime samples samplerate =
  secondsToTime(Int64.(to_int(div samples samplerate)))


let size filename =
  let sz = foi(stat filename).st_size in
  let (fsz, u) =
    if sz < 1e3 then (sz, "o") else
    if sz < 1e6 then ((sz /. 1e3), "Ko") else
    if sz < 1e9 then ((sz /. 1e6), "Mo") else
      ((sz /. 1e9), "Go")
  in Printf.sprintf "%.1f %s" fsz u


let checkPropertys file =

  if file.rate > Int64.zero then true
  else (
    let filename = filename file in

    file.fnode.size <- size filename;
    try
      let container = Av.open_input filename in
      let _, stream, codec = Av.find_best_audio_stream container in

      file.channels <- Avcodec.Audio.get_nb_channels codec;
      file.rate <- Int64.of_int(Avcodec.Audio.get_sample_rate codec);
      file.container <- Some container;

      let dur = Av.get_duration ~format:`Nanosecond stream in
      file.duration <- Int64.(div(mul dur file.rate) secondFractions);
      file.fnode.time <- samplesToTime file.duration file.rate;

      Av.get_input_metadata container |> List.iter(fun(k, v) ->
          let tag = String.lowercase_ascii k in

          if List.mem tag ["artist"; "album_artist"] then file.artist <- v else
          if List.mem tag ["album"] then file.album <- v else
          if List.mem tag ["title"] then file.title <- v else
          if List.mem tag ["genre"] then file.genre <- v
        );
      Av.close container;
      true
    with e -> ( traceRed(filename ^ " " ^ Printexc.to_string e); false)
  )


let progress file =
  if file.newPosition > Int64.zero then
    (samplesToTime file.newPosition file.rate) ^ " / " ^ file.fnode.time
  else file.fnode.time



let talker file =
  match file.voice with
  | None -> (
      let filename = filename file in
      let container = Av.open_input filename in
      let _, stream, codec = Av.find_best_audio_stream container in

      let input_resampler = InRsp.from_codec codec outChannelLayout outRate in
      let talker = {stream; input_resampler} in
      file.voice <- Some talker;
      talker)
  | Some talker -> talker


let stream file = (talker file).stream


let checkSeek file =

  if file.newPosition <> file.curPosition then (
    let p = Int64.(div(mul file.newPosition secondFractions) file.rate) in
    Av.seek(stream file) `Nanosecond p [||];
    file.curPosition <- file.newPosition;
    traceMagenta"Seek !";
    true
  )
  else false


let addToPosition file samples =

  if not (checkSeek file) then (
    file.newPosition <- Int64.(add file.curPosition (of_int samples));
    file.curPosition <- file.newPosition;
  );
  if file.duration > Int64.zero then (
    file.readPercent <- Int64.(to_int(div(mul file.newPosition hundred) file.duration))
  )

let setPositionPer10k file pos = Int64.(
    file.newPosition <- div(mul file.duration (of_int pos)) (of_int 10000);
    file.readPercent <- pos / 100
  )


let resetPosition file =
  file.newPosition <- Int64.zero;
  file.curPosition <- Int64.zero


let addIfAudioFile filename l =
  try
    let (path, name) = splitFilename filename in
    let p = S.rindex name '.' in
    let ext = S.sub name p (S.length name - p) in
    if true || L.mem ext ~set:audio_ext then (
      let fnode = makeNode name path in
      let f = {title = uk; artist = uk; album = uk; genre = uk; fnode;
               duration = Int64.zero;
               curPosition = Int64.zero; newPosition = Int64.zero; readPercent = 0;
	       rate = Int64.zero; channels = 1; container = None; voice = None}
      in fnode.kind <- File f;
      fnode::l)
    else l
  with Not_found -> l


let load filename excludedFiles =

  let rec check filename l =
    try
      if L.mem filename ~set:excludedFiles then l
      else (
    	if Sys.is_directory filename then addDir filename l
    	else addIfAudioFile filename l
      )
    with e -> (traceMagenta(Printexc.to_string e); l )

  and addDir dirname l =
    let sons = Sys.readdir dirname in
    match (A.fold_left ~f:(fun l fn ->
	if fn.[0] = '.' then l else check (dirname^"/"^fn) l) ~init:[] sons) with
    | [] -> l
    | cl -> let children = A.of_list cl in
      A.fast_sort ~cmp:(fun n1 n2 -> S.compare n1.name n2.name) children;
      let (path, name) = splitFilename dirname in
      let dnode = makeNode name path ~size:(soi(A.length children)) in
      let dir = {dnode; children} in dnode.kind <- Dir dir;
      A.iteri ~f:(fun i nd -> nd.idx <- i; nd.parent <- Some dir) children;
      dnode::l
  in
  let e = S.length filename - 1 in
  let filename = if filename.[e] = '/' then S.sub filename 0 e
    else filename in
  if Sys.file_exists filename then check filename [] else []


let next node roots =
  let rec nxt nd =
    let nidx = nd.idx + 1 in
    match nd.parent with
    | None -> if nidx < A.length roots then Some roots.(nidx)
      else (trace("Node "^nd.name^"orphelin"); None)
    | Some pd -> if nidx < A.length pd.children then Some pd.children.(nidx)
      else nxt pd.dnode
  in
  nxt node


let iterFiles fct lst =
  let rec itr nd =
    match nd.kind with
    | File f -> fct f
    | Dir d -> A.iter ~f:itr d.children
    | Null -> ()
  in
  A.iter ~f:itr lst


let concatChildren children newChildren =
  let offset = A.length children in
  A.iteri ~f:(fun i nd -> nd.idx <- i + offset) newChildren;
  A.append children newChildren


let addChildrenToDir children dir =
  let offset = A.length dir.children in
  A.iteri ~f:(fun i nd -> nd.idx <- i + offset; nd.parent <- Some dir) children;
  dir.children <- A.append dir.children children;
  dir.dnode.size <- soi(offset + A.length children)


let supNode node parent =

  let lg = A.length parent.children - 1
  in
  if lg < 0 || node.idx < 0 || node.idx > lg then ()
  else (
    parent.dnode.size <- soi lg;

    if node.idx = 0 then (
      parent.children <- A.sub parent.children ~pos:1 ~len:lg;
      A.iter ~f:(fun n -> n.idx <- n.idx - 1) parent.children
    )
    else if node.idx = lg then parent.children <- A.sub parent.children ~pos:0 ~len:lg
    else (
      let part2 = A.sub parent.children ~pos:(node.idx + 1) ~len:(lg - node.idx) in
      A.iter ~f:(fun n -> n.idx <- n.idx - 1) part2;
      parent.children <- A.append(A.sub parent.children ~pos:0 ~len:node.idx) part2
    )
  )


let supNodeByIndex idx nodes =
  let lg = A.length nodes - 1 in

  if lg < 0 || idx < 0 || idx > lg then nodes else
  if idx = 0 then (
    let res = A.sub nodes ~pos:1 ~len:lg in
    A.iter ~f:(fun n -> n.idx <- n.idx - 1) res; res) else
  if idx = lg then A.sub nodes ~pos:0 ~len:lg
  else (
    let part2 = A.sub nodes ~pos:(idx + 1) ~len:(lg - idx) in
    A.iter ~f:(fun n -> n.idx <- n.idx - 1) part2;
    A.append(A.sub nodes ~pos:0 ~len:idx) part2
  )


let copy node =
  let rec cp n =
    match n.kind with
    | File f -> (
	let nf = makeFile n.name n.path ~time:n.time ~title:f.title ~artist:f.artist
	    ~album:f.album ~genre:f.genre ~rate:f.rate ~channels:f.channels ~size:n.size in nf.fnode)
    | Dir d -> (
	let nd = makeDir ~path:n.path n.name in
	addChildrenToDir(A.map ~f:cp d.children) nd; nd.dnode)
    | Null -> (makeNode n.name n.path ~time:n.time ~size:n.size)
  in
  cp node


let close lst =
  iterFiles(fun f ->
      match f.container with
      | Some container -> Av.close container; f.container <- None; f.voice <- None
      | None -> ()) lst

