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

module Ev = AppEvent

let devicesColumns = new GTree.column_list
let devicesNameColumn = devicesColumns#add Gobject.Data.string

class c (gui:PlayoGui.mainWindow) (ctrl:Controler.c) =
	object (self)

	initializer
		Ev.addObserver (self :> Ev.observer);

		(* output devices tree view *)
		let devicesTreeview = gui#devicesTreeview in

	  let devsCol = GTree.view_column ~title:"Name"
      ~renderer:(GTree.cell_renderer_text [], ["text", devicesNameColumn]) ()
		in
		ignore(devicesTreeview#append_column devsCol);

		devicesTreeview#selection#set_mode`SINGLE;
		ignore(devicesTreeview#selection#connect#changed ~callback:self#onDeviceSelectionChanged);


(* observer methods *)
	method notify =	function
		| Ev.OutputDeviceChanged od -> self#update()
		| _ -> ()

	
	method onDeviceSelectionChanged() =
		let model = gui#devicesTreeview#model in

    match gui#devicesTreeview#selection#get_selected_rows with
		| [] -> ()
		| path::tl ->
      let row = model#get_iter path in
      let name = model#get ~row ~column:devicesNameColumn in
			ctrl#changeOutputDevice name


	method update() =

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

