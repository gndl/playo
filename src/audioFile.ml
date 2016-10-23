(* 
 * Copyright (C) 2015 Ga�tan Dubreil
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

open Usual

open Gstreamer

let audio_ext = [ ".mp3"; ".wav"; ".ogg"; ".flac"; ".pls"](*; ".m3u"*)
let audio_ext_pattern = L.map(fun ext -> "*"^ext) audio_ext

let uk = ""
let defRate = 44100

type state = Single | Repeat | Track | Random | Pause | Off

type t = {
	mutable title : string;
	mutable artist : string;
	mutable album : string;
	mutable genre : string;
	fnode : node;
	mutable currentPosition : Int64.t;
	mutable newPosition : Int64.t;
	mutable duration : Int64.t;
	mutable readPercent : int;
	mutable voice : Pipeline.t option
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
let currentPosition file = file.currentPosition
let newPosition file = file.newPosition
let readPercent file = file.readPercent
let voice file = file.voice
let name file = file.fnode.name
let path file = file.fnode.path
let filename file = file.fnode.path^file.fnode.name
let duration file = file.duration
let time file = file.fnode.time
let size file = file.fnode.size
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
	?(size = uk) name path =
		
	let fnode = {name; path; time; size; kind = Null; idx; parent; state; visible = true}
	in
	let file = {title; artist; album; genre; fnode; currentPosition = Int64.zero;
	newPosition = Int64.zero; duration = Int64.zero; readPercent = 0; voice}
	in
	fnode.kind <- File file; file



let unexistentNode = {name = uk; path = uk; time = uk; size = uk; kind = Null; idx = -1; parent = None; state = Off; visible = false}
let unexistentFile = {title = uk; artist = uk; album = uk; genre = uk;
	fnode = unexistentNode; currentPosition = Int64.zero; newPosition = Int64.zero; duration = Int64.zero; readPercent = 0; voice = None}
let unexistentDir = {dnode = unexistentNode; children = [||]}
(*let unexistent = Dir unexistentDir*)

let hasInfo f = f.title <> uk || f.artist <> uk || f.album <> uk || f.genre <> uk
let hasId f = f.title <> uk || f.artist <> uk || f.album <> uk

let stream file =
	match file.voice with
	| None -> (
	let readPipelineStatement = Printf.sprintf
		"filesrc location=\"%s\" ! decodebin ! audioconvert ! audioresample ! autoaudiosink"
		(filename file)
	in
(*  	traceCyan readPipelineStatement;*)
	  let stream = Pipeline.parse_launch readPipelineStatement in

		file.voice <- Some stream;
		stream
	)
	| Some stream -> stream


let secondesToTime secondes =
	let h = secondes / 3600 in
	let m = secondes / 60 - h * 60 in
	let s = secondes - m * 60 - h * 3600 in
	if h > 0 then Printf.sprintf " %d:%02d:%02d " h m s
	else Printf.sprintf " %d:%02d " m s

let framesSamplerateToTime frames samplerate =
	secondesToTime(Int64.to_int(Int64.div frames (Int64.of_int samplerate)))

let timeToHMS nsd =
  let sd = Int64.(to_int(div nsd (of_int 1_000_000_000))) in
  let h = sd / 3_600 in
  let m = (sd - (h * 3_600)) / 60 in
  let s = sd - (h * 3_600) - (m * 60) in
  let h, mw = if h = 0 then ("", 1) else (Printf.sprintf "%d:" h, 2) in 
  Printf.sprintf "%s%0*d:%02d" h mw m s

let timeToHMSMS nsd =
  let msd = Int64.(to_int(div nsd (of_int 1000000))) in
  let h = msd / 3600000 in
  let m = (msd - (h * 3600000)) / 60000 in
  let s = (msd - (h * 3600000) - (m * 60000)) / 1000 in
  let ms = msd - (h * 3600000) - (m * 60000) - (s * 1000) in
  let h = if h = 0 then "" else Printf.sprintf "%d:" h in 
  let ms = if ms = 0 then "" else Printf.sprintf ".%d" ms in 
  Printf.sprintf "%s%d:%d%s" h m s ms

let size filename =
  let stats = Unix.stat filename in
	let sz = foi(stats.st_size) in
	let (fsz, u) =
		if sz < 1000. then (sz, "o") else
		if sz < 1000000. then ((sz /. 1000.), "Ko") else
		if sz < 1000000000. then ((sz /. 1000000.), "Mo") else
		((sz /. 1000000000.), "Go")
	in 
  let sz = Printf.sprintf "%.1f %s" fsz u in
  sz


let checkProperties file =
	
	if file.duration <> Int64.zero || file.duration = Int64.zero then true
	else (
    let fileName = filename file in
    let pipeline = Printf.sprintf "filesrc location=\"%s\" ! decodebin ! fakesink" fileName in
    let bin = Pipeline.parse_launch pipeline in
		(*
	let bin = stream file in
*)
   Element.set_state bin Element.State_paused |> Log.stateChange;
(*		file.fnode.size <- size fileName;*)
  Element.get_state bin |> Log.states;

  file.duration <- Element.duration bin Format.Time;
  file.fnode.time <- timeToHMS file.duration;

		    let () = match Bus.(pop_filtered (of_element bin) [Message.Tag]) with
		| None -> ()
		| Some msg ->
			if Message.message_type msg = Message.Tag then (
        let tags = Message.parse_tag msg in
      
				  L.iter (fun (l,v) ->
					let vs = String.concat ", " v in
					traceBlue[l; " : "; vs];
					match l with
				| "artist" -> file.artist <- vs
    		| "album" -> file.album <- vs
    		| "title" -> file.title <- vs
    		| "genre" -> file.genre <- vs
        | _ -> ()
					) tags
		)
		in
   Element.set_state bin Element.State_null |> Log.stateChange;
  		true
		)


let progress file =
	if file.newPosition > Int64.zero then
      Printf.sprintf "%s / %s" (timeToHMS file.newPosition ) file.fnode.time
	else file.fnode.time


let setReadPercent file readPercent =
  file.currentPosition <- file.newPosition;
	file.readPercent <- readPercent


let setPosition file posPer10k =
		let adjustedPosPer10k = if posPer10k < 0 then 0 else
							 if posPer10k > 9999 then 9999 else posPer10k
    in
		file.readPercent <- adjustedPosPer10k / 100;
		let posPer10k = Int64.of_int adjustedPosPer10k in
		file.newPosition <- Int64.(div(mul file.duration posPer10k) (of_int 10000))


let stop file =
  match file.voice with
	| Some pipeline -> Element.(set_state pipeline State_null) |> Log.stateChange;
	| None -> ()


let close file =
  stop file;
  file.voice <- None


let addIfAudioFile filename l =
	try
	let (path, name) = splitFilename filename in
	let p = String.rindex name '.' in
	let ext = String.sub name p (String.length name - p) in
	if L.mem ext audio_ext then (
		let fnode = makeNode name path in
		let f = {title = uk; artist = uk; album = uk; genre = uk; fnode;
						 currentPosition = Int64.zero; newPosition = Int64.zero;
             duration = Int64.zero; readPercent = 0; voice = None}
		in fnode.kind <- File f;
		fnode::l)
	else l
	with Not_found -> l


let load filename excludedFiles =

	let rec check filename l =
		try
			if L.mem filename excludedFiles then l
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
				A.fast_sort(fun n1 n2 -> String.compare n1.name n2.name) children;
				let (path, name) = splitFilename dirname in
				let dnode = makeNode name path ~size:(soi(A.length children)) in
				let dir = {dnode; children} in dnode.kind <- Dir dir;
				A.iteri(fun i nd -> nd.idx <- i; nd.parent <- Some dir) children;
				dnode::l
	in
	let e = String.length filename - 1 in
	let filename = if filename.[e] = '/' then String.sub filename 0 e
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
			| Dir d -> A.iter itr d.children
			| Null -> ()
	in
	A.iter itr lst


let concatChildren children newChildren =
	let offset = A.length children in
	A.iteri(fun i nd -> nd.idx <- i + offset) newChildren;
	A.append children newChildren


let addChildrenToDir children dir =
	let offset = A.length dir.children in
	A.iteri(fun i nd -> nd.idx <- i + offset; nd.parent <- Some dir) children;
	dir.children <- A.append dir.children children;
	dir.dnode.size <- soi(offset + A.length children)


let supNode node parent =
	
	let lg = A.length parent.children - 1
	in
	if lg < 0 || node.idx < 0 || node.idx > lg then ()
	else (
		parent.dnode.size <- soi lg;
		
		if node.idx = 0 then (
			parent.children <- A.sub parent.children 1 lg;
			A.iter(fun n -> n.idx <- n.idx - 1) parent.children
		)
		else if node.idx = lg then parent.children <- A.sub parent.children 0 lg
		else (
			let part2 = A.sub parent.children (node.idx + 1) (lg - node.idx) in
			A.iter(fun n -> n.idx <- n.idx - 1) part2;
			parent.children <- A.append(A.sub parent.children 0 node.idx) part2
		)
	)


let supNodeByIndex idx nodes =
	let lg = A.length nodes - 1 in
	
	if lg < 0 || idx < 0 || idx > lg then nodes else
	if idx = 0 then (
		let res = A.sub nodes 1 lg in
		A.iter(fun n -> n.idx <- n.idx - 1) res; res) else
	if idx = lg then A.sub nodes 0 lg
	else (
		let part2 = A.sub nodes (idx + 1) (lg - idx) in
		A.iter(fun n -> n.idx <- n.idx - 1) part2;
		A.append(A.sub nodes 0 idx) part2
	)
	
	
let copy node =
	let rec cp n =
		match n.kind with
		| File f -> (
			let nf = makeFile n.name n.path ~time:n.time ~title:f.title ~artist:f.artist
			 ~album:f.album ~genre:f.genre ~size:n.size in nf.fnode)
		| Dir d -> (
			let nd = makeDir ~path:n.path n.name in
			addChildrenToDir(A.map cp d.children) nd; nd.dnode)
		| Null -> (makeNode n.name n.path ~time:n.time ~size:n.size)
	in
	cp node


let closeList lst = iterFiles close lst

(*
let iterFileLst fct lst =
	let rec itr l =
		match l with
			| [] -> ()
			| nd::t -> (
				match nd.kind with
					| File f -> fct f
					| Dir d -> itr d.children;
				itr t
			);
	in
	itr lst


let loadnode filename =

	let rec check filename l =
		try
		if Sys.is_directory filename then addDir filename l
		else addIfAudioFile filename (stat filename) l
		with e -> (traceMagenta(Printexc.to_string e); l )

	and addDir dirname l = (*trace("add dir "^dirname);*)
		let sons = Sys.readdir dirname in
		match (A.fold_left(fun l fn ->
			if fn.[0] = '.' then l else check (dirname^"/"^fn) l) [] sons) with
			| [] -> l
			| cl -> let (path, name) = splitFilename dirname in
				{name = name; path = path; kind = Dir{nodes = cl}}::l
	in
	if Sys.file_exists filename then check filename [] else []


let isAudioFile filename =
	if filename.[0] = '.' then false else (
		try
		let p = String.rindex filename '.' in
		let ext = String.sub filename p (String.length filename - p) in
	
		if L.mem ext audio_ext then true else false
	
		with Not_found -> false
	)


let loadt filename =

	let rec check idx filename =
		try
		if Sys.is_directory filename then addDir filename l
		else addIfAudioFile filename (stat filename) l
		with e -> (traceMagenta(Printexc.to_string e); l )

	and addDir dirname l = (*trace("add dir "^dirname);*)
		let ca = Sys.readdir dirname in
		let cl = A.fold_left(fun l fn -> if fn.[0] = '.' then l else fn::l) [] ca
		in
		let (dl, fl) = L.partition(fun n -> Sys.is_directory n) cl in
		let dl = L.fast_sort String.compare dl in
		let fl = L.fast_sort String.compare fl in
		let ca = A.of_list cl in
		let da = A.init

		let ca = A.mapi check 
		match () with
			| [] -> l
			| cl -> let (path, name) = splitFilename dirname in
				{node = {name = name; path = path; idx = ; kind = Dir{nodes = cl}}::l
	in
	if Sys.file_exists filename then check filename [] else []

let unixload filename =

	let rec check filename l =
		let st = (*LargeFile.*)stat filename in

		match st.st_kind with
			| S_REG -> addIfAudioFile filename st l
			| S_DIR -> addDir filename l
			| S_CHR -> trace(filename^"is S_CHR"); l
			| S_BLK -> trace(filename^"is S_BLK"); l
			| S_LNK -> trace(filename^"is S_LNK"); l
			| S_FIFO -> trace(filename^"is S_FIFO"); l
			| S_SOCK -> trace(filename^"is S_SOCK"); l

	and addDir dirname l = trace("add dir "^dirname);
		let h = opendir dirname in

		let rec rddr l =
			let fn = try readdir h with End_of_file -> (closedir h; "") in
			if fn = "" then l else rddr(check fn l);
		in
		match rddr [] with
			| [] -> l
			| cl -> let (path, name) = splitFilename dirname in
				{name = name; path = path; kind = Dir{nodes = cl}}::l
	in
	check filename []
*)


(*
let nodeOf = function File f -> f.node | Dir d -> d.node
let nameOf t = (nodeOf t).name
let pathOf t = (nodeOf t).path
let idxOf t = (nodeOf t).idx
let sizeOf t = (nodeOf t).size
let parentOf t = (nodeOf t).parent


*)

