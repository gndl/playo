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

open Usual
open AudioFile

module Ev = EventBus

type playlist = {label:string; files:string list}


class c (filesModel:FilesModel.c) ?(filenameList = []) () =
  object (self)

    val mPlayer = Player.make()
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
      filesModel#setNodes mNodes;

      Configuration.addFiles filenameList;

      mPlayer#setVolume(Configuration.getVolume());
      self#addFiles ~save:false (Configuration.getFiles());

      L.iter ~f:(fun (playlistName, files) -> self#newPlaylist playlistName;
	          L.iter ~f:(fun (fileName, filePath) ->
		      self#addNode(AudioFile.makeFile fileName filePath).fnode;
	            ) files;
	        ) (Configuration.getPlaylists());

      let outputDevice = Configuration.getOutputDevice()
      in

      if L.mem outputDevice ~set:mPlayer#getOutputDevices then (
        mPlayer#changeOutputDevice outputDevice;
      )
      else (
        Configuration.setOutputDevice mPlayer#getOutputDevice;
      )


    method release() =
      AudioFile.close mNodes;

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
      | Ev.StartFile f -> f.fnode.state <- mPlayMode;
        L.iter ~f:(fun nd -> nd.state <- mPlayMode) mSelectedNodes;
      | Ev.PauseFile f -> f.fnode.state <- Pause;
        L.iter ~f:(fun nd -> nd.state <- Pause) mSelectedNodes;
      | Ev.EndFile f -> f.fnode.state <- Off
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


    method play = mPlayer#play;
    method pause = mPlayer#pause;
    method stop = mPlayer#stop;

    method setVolume volumePercent =
      mPlayer#setVolume volumePercent;
      Configuration.setVolume volumePercent;

    method getVolume = Configuration.getVolume()


    method checkPropertys file =
      try AudioFile.checkPropertys file
      with Unix.Unix_error(e, f, p) -> (
	  let msg = (Unix.error_message e)^" ("^f^" "^p^")" in
	  Ev.notify(Ev.Error msg);
	  false
        )


    method checkVoice file =
      if self#checkPropertys file then (
        try
	  let _ = AudioFile.stream file in true
        with _ -> (
	    Ev.notify(Ev.Error "Error");
	    false
	  )
      ) else false

    method addFiles ?(save = true) filenameList =
      let ll = L.map ~f:(fun fn -> trace("add "^fn);
		          AudioFile.load fn (Configuration.getHiddenFiles())) filenameList
      in
      let children = A.of_list(L.flatten ll) in
      AudioFile.addChildrenToDir children mFolders;
      A.iter ~f:(fun nd -> filesModel#fileInserted nd) children;

      if save then Configuration.addFiles filenameList;


    method setPlayMode playMode = mPlayMode <- playMode


    method changeFiles nodes =

      let selectionIsSame = try L.for_all2 ~f:(==) mSelectedNodes nodes
        with Invalid_argument _ -> false
      in
      if selectionIsSame then (
        if mPlayer#isPlaying then mPlayer#pause else mPlayer#play;
      )
      else (
        L.iter ~f:(fun nd -> nd.state <- Off) mSelectedNodes;
        L.iter ~f:(fun nd -> nd.state <- mPlayMode) nodes;
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


    method setFilePosition file posPer10k =

      let adjustedPosPer10k = if posPer10k < 0 then 0 else
        if posPer10k > 9999 then 9999 else posPer10k
      in
      AudioFile.setPositionPer10k file adjustedPosPer10k


    method openPlaylist playlistName = trace playlistName
    method savePlaylist() = ()
    method savePlaylistAs filename = trace filename
    method setPlaylistName playlistName = mPlaylistName <- playlistName

    method playlistExist = mCurPlaylist != AudioFile.unexistentDir

    method newPlaylist name =
      mCurPlaylist <- AudioFile.makeDir name;
      AudioFile.addChildrenToDir [|mCurPlaylist.dnode|] mPlaylists;
      filesModel#fileInserted mCurPlaylist.dnode;


    method addNode node =
      if node == mFolders.dnode then (
        Ev.notify Ev.AddFolder;
      )
      else (
        let nbPlaylist = A.length mPlaylists.children
        in
        if node == mPlaylists.dnode || nbPlaylist = 0 then (
	  Ev.notify(Ev.NewPlaylist("Playlist "^soi(nbPlaylist + 1)));
        );

        let rec mkFileLst fl nd =
	  match nd.kind with
	  | File _f -> nd::fl
	  | Dir d -> A.fold_left ~f:mkFileLst ~init:fl d.children
	  | Null -> fl
        in
        if node != mPlaylists.dnode
        && mCurPlaylist != AudioFile.unexistentDir then (
	  let lst = L.rev(mkFileLst [] node) in
	  let children = A.of_list(L.map ~f:AudioFile.copy lst) in

          AudioFile.addChildrenToDir children mCurPlaylist;

          A.iter ~f:(fun nd -> filesModel#fileInserted nd) children;
        )
      )

    method supNode node =
      if node != mPlaylists.dnode && node != mFolders.dnode then (

        let fullFileName = node.path ^ node.name in
        traceYellow("Sup node "^fullFileName);

        filesModel#fileDeleted node;
        (* Ev.notify(Ev.SupFile node); *)

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

let make (filesModel:FilesModel.c) ?(filenameList = []) () =
  new c filesModel ~filenameList ()
