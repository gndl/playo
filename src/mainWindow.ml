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
open GuiUtility
open AudioFile
open FilesModel

module Ev = AppEvent

(*use "gui.ml";;*)

class c (ctrl:Controler.c) = object (self) inherit PlayoGui.mainWindow()

	method openFile() =
		match selectFile toplevel audioFilter with
			| Some fl -> ctrl#addFiles fl
			| None -> ()
(*
	method openPlaylist () =
		match selectFile toplevel sessionFilter with
			| Some fl -> self#mainWindow#set_title(f^" : "^appName); ctrl#openPlaylist f
			| None -> ()
*)
	method savePlaylistAs () =
		match saveFile toplevel sessionFilter with
			| Some f -> self#mainWindow#set_title(f^" : "^appName); ctrl#savePlaylistAs f
			| None -> ()

	method quit () =
		ctrl#stop;
		trace "quit";
		GMain.quit()


	method play() = if playButton#get_active then ctrl#play else ctrl#stop
	method pause = ctrl#pause
	method stop = ctrl#stop
	method changeVolume() = ctrl#changeVolume self#volumeScale#adjustment#value

	method filter() = Ev.notify(Ev.Filter searchEntry#text); trace searchEntry#text

		
	initializer
		Ev.addObserver (self :> Ev.observer);

	  self#bind ~name:"on_playButton_clicked" ~callback:self#play;
	  self#bind ~name:"on_searchEntry_changed" ~callback:self#filter;
	  self#bind ~name:"on_volumeScale_value_changed" ~callback:self#changeVolume;
	  self#bind ~name:"on_openFileButton_clicked" ~callback:self#openFile;
(*
	  self#bind ~name:"on_newPlaylistToolbarButton_clicked" ~callback:ctrl#newPlaylist;
	  self#bind ~name:"on_savePlaylistToolbarButton_clicked" ~callback:ctrl#savePlaylist;
	  self#bind ~name:"on_savePlaylistAsToolbarButton_clicked" ~callback:self#savePlaylistAs;

	  self#bind ~name:"on_pauseButton_clicked" ~callback:self#pause;
	  self#bind ~name:"on_stopButton_clicked" ~callback:self#stop;
	  self#bind ~name:"on_beginButton_clicked" ~callback:self#toBegin;
	  self#bind ~name:"on_playToButton_clicked" ~callback:self#playTo;
	  self#bind ~name:"on_tickSpinButton_input" ~callback:self#tickEntered;
*)
	  ignore(self#mainWindow#connect#destroy ~callback:self#quit);
		

(* observer methods *)
	method notify =	function
(*		| Ev.State s -> ()*)
	| Ev.FileChanged file ->
		trackProgressbar#set_fraction(foi(AudioFile.readPercent file) /. 100.);
		trackProgressbar#set_text(AudioFile.progress file)
	| Ev.StartFile f -> (
		if AudioFile.hasId f then
			self#mainWindow#set_title(f.artist^" : "^f.title^" ("^f.album^") - "^appName)
(*			else self#mainWindow#set_title(f.fnode.path^f.fnode.name^" - "^appName);*)
		else self#mainWindow#set_title(f.fnode.name^" - "^appName);
	)
	| Ev.NewPlaylist defName -> (
			let e = GuiUtility.getEntry toplevel "Enter new playlist name" defName in
			match e with Some n -> ctrl#newPlaylist n | None -> ()
		);
	| Ev.AddFolder -> self#openFile()
	| Ev.Volume v -> self#volumeScale#adjustment#set_value v
	| Ev.Error msg -> showErrorMessage toplevel msg
	| _ -> ()

end

