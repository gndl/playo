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

module Ev = EventBus

let devicesColumns = new GTree.column_list
let devicesNameColumn = devicesColumns#add Gobject.Data.string

let hiddenFilesColumns = new GTree.column_list
let hiddenFilesPathColumn = hiddenFilesColumns#add Gobject.Data.string

class c (gui:PlayoGui.mainWindow) (ctrl:Controler.c) =
	object (self)
	val mAddCol = GTree.view_column
								~renderer:(GTree.cell_renderer_pixbuf[`STOCK_ID "gtk-add"], []) ()

	initializer
		Ev.addObserver self#observe;

		let txtRndrr = GTree.cell_renderer_text [] in

		(* hidden files tree view *)
		let selection = gui#hiddenFilesTreeview#selection in

		let pathCol = GTree.view_column ~title:"Path"
									~renderer:(txtRndrr, ["text", hiddenFilesPathColumn]) ()
		in
		pathCol#set_resizable true;
		ignore(gui#hiddenFilesTreeview#append_column pathCol);
(*		ignore(gui#hiddenFilesTreeview#append_column mAddCol);
*)
		selection#set_mode`SINGLE;
		ignore(selection#connect#changed ~callback:self#onHiddenFilesSelectionChanged);

		(* output devices tree view *)
		let selection = gui#devicesTreeview#selection in

	  let devsCol = GTree.view_column ~title:"Name"
									~renderer:(txtRndrr, ["text", devicesNameColumn]) ()
		in
		ignore(gui#devicesTreeview#append_column devsCol);

		selection#set_mode`SINGLE;
		ignore(selection#connect#changed ~callback:self#onDeviceSelectionChanged);
		
(*		self#update()
*)

	(* observer methods *)
	method observe =	function
		| Ev.HiddenFilesChanged -> self#updateHiddenFiles()
		| Ev.OutputDeviceChanged od -> self#updateOutputDevices()
		| _ -> ()

	
	method onHiddenFilesSelectionChanged() =
		let model = gui#hiddenFilesTreeview#model in

    match gui#hiddenFilesTreeview#selection#get_selected_rows with
		| [] -> ()
		| path::tl ->
      let row = model#get_iter path in
      let filePath = model#get ~row ~column:hiddenFilesPathColumn in
			ctrl#restoreHiddenFile filePath


	method onDeviceSelectionChanged() =
		let model = gui#devicesTreeview#model in

    match gui#devicesTreeview#selection#get_selected_rows with
		| [] -> ()
		| path::tl ->
      let row = model#get_iter path in
      let name = model#get ~row ~column:devicesNameColumn in
			ctrl#changeOutputDevice name



	method update() =
		self#updateHiddenFiles();
		self#updateOutputDevices();


	method updateHiddenFiles() =
		(* update hidden files tree view *)
		let hiddenFiles = Configuration.getHiddenFiles() in
		let hiddenFilesModel = GTree.list_store hiddenFilesColumns in

		if L.length hiddenFiles > 0 then (

			L.iter ~f:(fun filePath ->
  			let row = hiddenFilesModel#append () in
  			hiddenFilesModel#set ~row ~column:hiddenFilesPathColumn filePath;
  		)
  			hiddenFiles;

			gui#hiddenFilesLabel#misc#show();
			gui#hiddenFilesTreeview#misc#show();
		)
		else (
			gui#hiddenFilesLabel#misc#hide();
			gui#hiddenFilesTreeview#misc#hide();
		);

		gui#hiddenFilesTreeview#set_model(Some (hiddenFilesModel#coerce));


	method updateOutputDevices() =
		(* update output devices tree view *)
		let currDev = ctrl#getOutputDevice in
		let devicesModel = GTree.list_store devicesColumns in

		let currRow = L.fold_left ~f:(fun currRow name ->
			let row = devicesModel#append () in
			devicesModel#set ~row ~column:devicesNameColumn name;
			
			if name = currDev then Some row else currRow
		)
			~init:None ctrl#getOutputDevices
		in

		gui#devicesTreeview#set_model(Some (devicesModel#coerce));
	
		match currRow with None -> ()
		| Some row -> gui#devicesTreeview#selection#select_iter row


end

