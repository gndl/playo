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
open AudioFile

module Ev = EventBus

type playlist = {label:string; files:string list}


class c ?(filenameList = []) () = object (self)

	val mPlayer = Player.make
	val mutable mPlayMode = AudioFile.Single
	val mPlaylists = AudioFile.makeDir "Playlists"
	val mFolders = AudioFile.makeDir ~idx:1 "Folders"
	val mutable mNodes : AudioFile.node array = [||]
	val mutable mSelectedNodes : AudioFile.node list = []
	val mutable mPlaylist = {label = ""; files = []}
	val mutable mCurPlaylist = AudioFile.unexistentDir
	val mutable mPlaylistName = ""


	initializer
		Ev.addObserver self#observe;
		mNodes <- [|mPlaylists.dnode; mFolders.dnode|];
		Configuration.addFiles filenameList;


	method init() =
		mPlayer#setVolume(Configuration.getVolume());
		self#addFiles ~save:false (Configuration.getFiles());
		
		L.iter(fun (playlistName, files) -> self#newPlaylist playlistName;
			L.iter(fun (fileName, filePath) ->
				self#addNode(AudioFile.makeFile fileName filePath).fnode;
			) files;
		) (Configuration.getPlaylists());

		let outputDevice = Configuration.getOutputDevice()
		in

		if L.mem outputDevice ~set:mPlayer#getOutputDevices then
			mPlayer#changeOutputDevice outputDevice
		else
			Configuration.setOutputDevice mPlayer#getOutputDevice


	method release() =
		AudioFile.closeList mNodes;
		
		Configuration.setPlaylists(A.fold_right ~f:(fun pl pls ->
			match pl.kind with
			| Dir d -> (pl.name, A.fold_right ~f:(fun f fs ->
				(f.name, f.path) :: fs) d.children ~init:[]) :: pls
			| _ -> [("ERROR", [])]
		) mPlaylists.children ~init:[]);
		
		Configuration.save();
		trace "Bye"

(* observer methods *)
	method observe =	function
(*		| Ev.State s when s = State.Stop -> AudioFile.close mNodes*)
		| Ev.StartFile f -> f.fnode.state <- mPlayMode;
				L.iter(fun nd -> nd.state <- mPlayMode) mSelectedNodes;
(*
			match f.fnode.state with
			| Off -> f.fnode.state <- if mPlayMode = Repeat then Single else mPlayMode
			| _ -> ()*)
		| Ev.PauseFile f -> f.fnode.state <- Pause;
				L.iter(fun nd -> nd.state <- Pause) mSelectedNodes;
		| Ev.EndFile f -> f.fnode.state <- Off(*
			match f.fnode.state with
			| Repeat -> ()
			| _ -> f.fnode.state <- Off*)
		| Ev.EndList f -> (
				match mPlayMode with
				| Repeat -> ()
				| Track -> (
					match AudioFile.next f.fnode mNodes with
					| Some nd -> self#changeFiles [nd]
					| None -> self#changeFiles []
				)
				| _ -> self#changeFiles []
			)
		| Ev.OutputDeviceChanged od -> Configuration.setOutputDevice od
		| _ -> ()

(*
	method getState = Browser.state mPlayer
	method setState s = Browser.state mPlayer <- s; L.iter(fun o -> o#seeState s) mObservers
*)
(*	method player = mPlayer*)
	method nodes = mNodes

	method play = mPlayer#play;
	method pause = 	mPlayer#pause;
	method stop = mPlayer#stop;
		
	method setVolume volumePercent =
		mPlayer#setVolume volumePercent;
		Configuration.setVolume volumePercent;

	method getVolume = Configuration.getVolume()

		
	method checkProperties file =
		try AudioFile.checkProperties file
		with Unix.Unix_error(e, f, p) -> (
			let msg = (Unix.error_message e)^" ("^f^" "^p^")" in
			Ev.notify(Ev.Error msg);
			false
		)


	method checkVoice file =
		self#checkProperties file
(*
*)
(*
		if self#checkProperties file then (
			let _ = AudioFile.stream file in true
		) else false
*)

	method addFiles ?(save = true) filenameList =
		let ll = L.map(fun fn -> trace("add "^fn);
			AudioFile.load fn (Configuration.getHiddenFiles())) filenameList
		in
		let children = A.of_list(L.flatten ll) in
		AudioFile.addChildrenToDir children mFolders;
		A.iter(fun nd -> Ev.notify(Ev.AddFile nd)) children;
		(*mNodes <- AudioFile.concatChildren mNodes (A.of_list(L.flatten ll));*)
		if save then Configuration.addFiles filenameList;


	method setPlayMode playMode = mPlayMode <- playMode
	
	
	method changeFiles nodes =

		let selectionIsSame = try L.for_all2(==) mSelectedNodes nodes 
												with Invalid_argument e -> false
		in
		if selectionIsSame then (
			if mPlayer#isPlaying then mPlayer#pause else mPlayer#play;
		)
		else (
			L.iter(fun nd -> nd.state <- Off) mSelectedNodes;
			L.iter(fun nd -> nd.state <- mPlayMode) nodes;
			mSelectedNodes <- nodes;
			
			let rec mkFileLst fl nd =

				match nd.kind with
				| File f -> if self#checkVoice f then f::fl else fl
				| Dir d -> (
					(match nd.parent with (* if the dir is a playlist, define it as the current playlist *)
					| Some p -> if p == mPlaylists then mCurPlaylist <- d;
					| None -> ());
					A.fold_left ~f:mkFileLst ~init:fl d.children)
				| Null -> traceRed("Node "^nd.name^"'s kind is NULL!"); fl
			in
			let fileLst = A.fold_left ~f:mkFileLst ~init:[] (A.of_list nodes) in
			mPlayer#changeFiles(L.rev(fileLst))
		)


	method setFilePosition file posPer10k = AudioFile.setPosition file posPer10k

	method openPlaylist playlistName = trace playlistName
	method savePlaylist() = ()
	method savePlaylistAs filename = trace filename
	method setPlaylistName playlistName = mPlaylistName <- playlistName
	
	method playlistExist = mCurPlaylist != AudioFile.unexistentDir

	method newPlaylist name =
		mCurPlaylist <- AudioFile.makeDir name;
		AudioFile.addChildrenToDir [|mCurPlaylist.dnode|] mPlaylists;
		Ev.notify(Ev.AddFile mCurPlaylist.dnode);


	method addNode node =
		if node == mFolders.dnode then (
			Ev.notify Ev.AddFolder;
		)
		else (
			let nbPlaylist = A.length mPlaylists.children
			in
			if node == mPlaylists.dnode || nbPlaylist = 0 then
				Ev.notify(Ev.NewPlaylist("Playlist "^soi(nbPlaylist + 1)));

			let rec mkFileLst fl nd =
				match nd.kind with
				| File f -> nd::fl
				| Dir d -> A.fold_left ~f:mkFileLst ~init:fl d.children
				| Null -> fl
			in
			if node != mPlaylists.dnode
				&& mCurPlaylist != AudioFile.unexistentDir then (
				let lst = L.rev(mkFileLst [] node) in
				let children = A.of_list(L.map AudioFile.copy lst) in
				AudioFile.addChildrenToDir children mCurPlaylist;
				A.iter(fun nd -> Ev.notify(Ev.AddFile nd)) children;
			)
		)

	method supNode node =
		if node != mPlaylists.dnode && node != mFolders.dnode then (
			
			let fullFileName = node.path ^ node.name in
			traceYellow("Sup node "^fullFileName);
			
			Ev.notify(Ev.SupFile node);

			match node.parent with
			| Some p -> AudioFile.supNode node p;

				if p == mFolders then Configuration.removeFile fullFileName
				else (
					match p.dnode.parent with
					| Some pp -> if pp != mPlaylists then (
						Configuration.addHiddenFile fullFileName;
						Ev.notify(Ev.HiddenFilesChanged);
					)
					| None -> Configuration.removeFile fullFileName;
				)
			| None -> mNodes <- AudioFile.supNodeByIndex node.idx mNodes;
				Configuration.removeFile fullFileName
		);


	method restoreHiddenFile filePath =
		Configuration.removeHiddenFile filePath;
		self#addFiles ~save:false [filePath];
		Ev.notify(Ev.HiddenFilesChanged);



	method getOutputDevice = mPlayer#getOutputDevice
	method getOutputDevices = mPlayer#getOutputDevices
	method changeOutputDevice name =
		Configuration.setOutputDevice name;
		mPlayer#changeOutputDevice name 
	
end

let make ?(filenameList = []) () =
	let ctrl = new c ~filenameList () in
	ctrl#addFiles filenameList;
	ctrl
