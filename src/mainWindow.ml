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
(*open GuiUtility*)
open AudioFile
open FilesModel

module Ev = EventBus

(*use "gui.ml";;*)

class c (gui:PlayoGui.mainWindow) (ctrl:Controler.c) =
	object (self)

	val mControlPanel = new ControlPanel.c gui ctrl

	initializer
		Ev.addObserver self#observe;

	  gui#bind ~name:"on_playButton_clicked" ~callback:self#play;
	  gui#bind ~name:"on_searchEntry_changed" ~callback:self#filter;
	  gui#bind ~name:"on_volumeScale_value_changed" ~callback:self#changeVolume;
	  gui#bind ~name:"on_openFileButton_clicked" ~callback:self#openFile;
	  gui#bind ~name:"on_preferencesToggletoolbutton_toggled" ~callback:self#configuration;
(*
	  gui#bind ~name:"on_newPlaylistToolbarButton_clicked" ~callback:ctrl#newPlaylist;
	  gui#bind ~name:"on_savePlaylistToolbarButton_clicked" ~callback:ctrl#savePlaylist;
	  gui#bind ~name:"on_savePlaylistAsToolbarButton_clicked" ~callback:self#savePlaylistAs;

	  gui#bind ~name:"on_pauseButton_clicked" ~callback:self#pause;
	  gui#bind ~name:"on_stopButton_clicked" ~callback:self#stop;
	  gui#bind ~name:"on_beginButton_clicked" ~callback:self#toBegin;
	  gui#bind ~name:"on_playToButton_clicked" ~callback:self#playTo;
	  gui#bind ~name:"on_tickSpinButton_input" ~callback:self#tickEntered;
*)
	  ignore(gui#mainWindow#connect#destroy ~callback:self#quit);
		
		gui#volumeScale#adjustment#set_value ctrl#getVolume
		
		
	method init() = ()

(* observer methods *)
	method observe =	function
(*		| Ev.State s -> ()*)
	| Ev.FileChanged file ->
		gui#trackProgressbar#set_fraction(foi(AudioFile.readPercent file) /. 100.);
		gui#trackProgressbar#set_text(AudioFile.progress file)
	| Ev.StartFile f -> (
		if AudioFile.hasId f then
			gui#mainWindow#set_title(f.artist^" : "^f.title^" ("^f.album^") - "^appName)
(*			else gui#mainWindow#set_title(f.fnode.path^f.fnode.name^" - "^appName);*)
		else gui#mainWindow#set_title(f.fnode.name^" - "^appName);
	)
	| Ev.NewPlaylist defName -> (
			let e = GuiUtility.getEntry gui#toplevel "Enter new playlist name" defName in
			match e with Some n -> ctrl#newPlaylist n | None -> ()
		);
	| Ev.AddFolder -> self#openFile()
	| Ev.Error msg -> traceRed("Error : "^msg);
										GuiUtility.showErrorMessage gui#toplevel msg
	| _ -> ()


	method openFile() =
		match GuiUtility.(selectFile gui#toplevel audioFilter) with
			| Some fl -> ctrl#addFiles fl
			| None -> ()
(*
	method openPlaylist () =
		match selectFile toplevel sessionFilter with
			| Some fl -> gui#mainWindow#set_title(f^" : "^appName); ctrl#openPlaylist f
			| None -> ()
*)
	method savePlaylistAs () =
		match GuiUtility.(saveFile gui#toplevel sessionFilter) with
			| Some f -> gui#mainWindow#set_title(f^" : "^appName); ctrl#savePlaylistAs f
			| None -> ()

	method quit () =
		ctrl#stop;
		trace "quit";
		GMain.quit()


	method play() = if gui#playButton#get_active then ctrl#play else ctrl#stop
	method pause = ctrl#pause
	method stop = ctrl#stop
	method changeVolume() = ctrl#setVolume gui#volumeScale#adjustment#value

	method filter() = Ev.notify(Ev.Filter gui#searchEntry#text); trace gui#searchEntry#text

	method configuration () =
		if gui#configurationHbox#misc#visible then
			gui#configurationHbox#misc#hide()
		else (
			mControlPanel#update();
			gui#configurationHbox#misc#show()
		)
		
end

