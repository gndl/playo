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
open StdLabels
open AudioFile
open FilesModel

class c (filesModel:FilesModel.c) (treeView:GTree.view) (ctrl:Controler.c) toplevel =
	let nullColumn = GTree.view_column() in
	object (self)
	val mutable mColumns = ["Add"; "Name"; "Title"; "Artist"; "Album"; "Genre"; "Time"; "Size"; "Path"; "Sup"]
	val mutable mAddCol = nullColumn
	val mutable mNameCol = nullColumn
	val mutable mTitleCol = nullColumn
	val mutable mArtistCol = nullColumn
	val mutable mAlbumCol = nullColumn
	val mutable mGenreCol = nullColumn
	val mutable mTimeCol = nullColumn
	val mutable mSizeCol = nullColumn
	val mutable mPathCol = nullColumn
	val mutable mSupCol = nullColumn
	val mutable mButtonPressed = false
	val mutable mControlKeyPressed = false
	val mutable mShiftKeyPressed = false
	val mutable mAltKeyPressed = false

	val mutable mFilterMotif = ""
	val mutable mFilterRegexp = Str.regexp ""
	val filteredFilesModel = GTree.model_filter (filesModel :> GTree.model)
	

	initializer
		Ev.addObserver (self :> Ev.observer);

		treeView#set_model(Some (filteredFilesModel :> GTree.model));
		treeView#set_headers_clickable true;
		treeView#set_reorderable true;
		treeView#selection#set_mode`MULTIPLE;

		ignore(treeView#event#connect#button_press ~callback:self#buttonPressed);
		ignore(treeView#event#connect#button_release ~callback:self#buttonReleased);
(*		ignore(treeView#event#connect#key_press ~callback:self#keyPressed);
		ignore(treeView#event#connect#key_release ~callback:self#keyReleased);
*)
		ignore(treeView#connect#row_activated ~callback:self#activeHoverMode);
	  ignore(treeView#connect#cursor_changed ~callback:self#changeFiles);
	  (*ignore(treeView#connect#move_cursor ~callback:(fun step ->trace"on_filesView_move_cursor"));
	  ignore(treeView#connect#select_cursor_row ~callback:(fun()->trace"on_filesView_select_cursor_row"));
	  ignore(treeView#connect#toggle_cursor_row ~callback:(fun()->trace"on_filesView_toggle_cursor_row"));
*)
		let txtRndrr = GTree.cell_renderer_text [] in

		let pixRndrr = GTree.cell_renderer_pixbuf [`STOCK_ID "gtk-add"] in

		let addColumn columnName =
			
			if columnName = "Add" then (
    		mAddCol <- GTree.view_column ~renderer:(pixRndrr,[]) ();
    		ignore(treeView#append_column mAddCol);
			)
			else if columnName = "Name" then (
    		let sttRndrr = GTree.cell_renderer_pixbuf [`STOCK_ID "gtk-media-play"; `VISIBLE false] in
    (*		let col = GTree.view_column ~title:"Name" ~renderer:(txtRndrr,["text", nameColumn])() in*)
    		mNameCol <- GTree.view_column ~title:"Name"();
    		mNameCol#pack sttRndrr;
    		mNameCol#set_cell_data_func sttRndrr (self#renderState sttRndrr);
    		mNameCol#pack txtRndrr;
    		mNameCol#add_attribute txtRndrr "text" nameColumn;
    		mNameCol#set_resizable true;
				ignore(treeView#append_column mNameCol);
    		treeView#set_expander_column (Some mNameCol);
			)
			else if columnName = "Title" then (
    		mTitleCol <- GTree.view_column ~title:"Title" ~renderer:(txtRndrr,["text", titleColumn]) ();
    		mTitleCol#set_resizable true;
				ignore(treeView#append_column mTitleCol);
			)
			else if columnName = "Artist" then (
    		mArtistCol <- GTree.view_column ~title:"Artist" ~renderer:(txtRndrr,["text", artistColumn]) ();
    		mArtistCol#set_resizable true;
				ignore(treeView#append_column mArtistCol);
			)
			else if columnName = "Album" then (
    		mAlbumCol <- GTree.view_column ~title:"Album" ~renderer:(txtRndrr,["text", albumColumn]) ();
    		mAlbumCol#set_resizable true;
				ignore(treeView#append_column mAlbumCol);
			)
			else if columnName = "Genre" then (
    		mGenreCol <- GTree.view_column ~title:"Genre" ~renderer:(txtRndrr,["text", genreColumn]) ();
    		mGenreCol#set_resizable true;
				ignore(treeView#append_column mGenreCol);
			)
			else if columnName = "Time" then (
    		let prgRndrr = GTree.cell_renderer_progress [] in
    		mTimeCol <- GTree.view_column ~title:"Time" ~renderer:(prgRndrr,["text", timeColumn]) ();
    (*	col#add_attribute rndrr "value" readPercentColumn;*)
    		mTimeCol#set_cell_data_func prgRndrr (self#renderProgress prgRndrr);
    		mTimeCol#set_resizable true;
(*				col#set_sizing `AUTOSIZE;*)
				ignore(treeView#append_column mTimeCol);
			)
			else if columnName = "Size" then (
    		mSizeCol <- GTree.view_column ~title:"Size" ~renderer:(txtRndrr,["text", sizeColumn]) ();
    		mSizeCol#set_resizable true;
				ignore(treeView#append_column mSizeCol);
			)
			else if columnName = "Path" then (
    		mPathCol <- GTree.view_column ~title:"Path" ~renderer:(txtRndrr,["text", pathColumn]) ();
    		mPathCol#set_resizable true;
				ignore(treeView#append_column mPathCol);
			)
			else if columnName = "Sup" then (
    		let pixRndrr = GTree.cell_renderer_pixbuf [`STOCK_ID "gtk-close"] in
    		mSupCol <- GTree.view_column ~renderer:(pixRndrr,[]) ();
    		ignore(treeView#append_column mSupCol);
			)
		in
		
		L.iter addColumn (match Config.getColumns with
			| [] -> Config.setColumns mColumns; mColumns
			| l -> l);

(*
  treeView#selection#connect#after#changed ~callback:
    (fun () -> traceBlue "selection changed";
(*      L.iter treeView#selection#get_selected_rows ~f:
        (fun p -> match filesModel#custom_get_iter p with
				| Some row -> trace("file "^row.nd.name^" selected");(* ctrl#changeFile row.nd*)
				| None -> trace "Path not found !");
*)		);

	treeView#connect#after#row_activated ~callback:
    (fun path vcol -> traceBlue "Row activated";
(*       match filesModel#custom_get_iter path with
				| Some row -> trace("file "^row.nd.name^" selected"); ctrl#changeFile row.nd
				| None -> trace "Path not found !"
*)    );
*)
		filteredFilesModel#set_visible_func(fun model treeIter ->
			
			if S.length mFilterMotif = 0 then true
			else (
    		let path = model#get_path treeIter in
    		match filesModel#custom_get_iter path with
				| None -> false
    		| Some node -> node.visible
			)
		)


	method init() = trace "Init FilesView";
	
	method removeRow () = trace "remove row";
	
	method popupMenu ev =
  	let menu = GMenu.menu () in
  	let menuItemRemove = GMenu.menu_item ~label:"Remove" ~packing:menu#append () in
  	ignore(menuItemRemove#connect#activate ~callback:self#removeRow);
  	menu#popup ~button:(GdkEvent.Button.button ev) ~time:(GdkEvent.Button.time ev)

	method setFilterMotif v =
		mFilterMotif <- v;
		mFilterRegexp <- Str.regexp(S.lowercase v);
		filesModel#filter mFilterRegexp;
		filteredFilesModel#refilter()

	method changeFiles() = trace("on_filesView_cursor_changed");

		if treeView#hover_selection && mButtonPressed then (
			treeView#set_hover_selection false;
			treeView#set_hover_expand false;
			treeView#selection#set_mode`MULTIPLE;
		)
		else (
			let rowList = ListLabels.fold_left ~init:[]
				~f:(fun rl path -> match filesModel#custom_get_iter path with
					| Some r -> trace("row "^r.name^" selected"); r::rl
					| None -> traceRed "Path not found !"; rl)
				treeView#selection#get_selected_rows
			in
			ctrl#changeFiles rowList;
		)
(*
		let (selectionIsNew, rowList) = ListLabels.fold_left
			~f:(fun(sin, rl) path -> match filesModel#custom_get_iter path with
				| Some r -> trace("row "^r.name^" selected");
					((if r != mSelectedRow then true else sin), r::rl)
				| None -> traceRed "Path not found !"; (sin, rl))
			~init:(false, []) treeView#selection#get_selected_rows in
		if selectionIsNew then (
			try mSelectedRow <- L.hd rowList;
			ctrl#changeFiles rowList;
			with Failure e -> ()
		)
		else (
			if not treeView#hover_selection then
				if ctrl#player#isPlaying then ctrl#player#pause else ctrl#player#play;
		);*)


	method buttonPressed ev =
		mButtonPressed <- true;
		let x = int_of_float (GdkEvent.Button.x ev) in
		let y = int_of_float (GdkEvent.Button.y ev) in
		trace("on_filesView_button_press_event : x="^soi x^" y="^soi y);

		match treeView#get_path_at_pos ~x ~y with
		| None -> false
		| Some (path, col, colX, colY) -> (trace("col "^col#title^" id = "^soi col#get_sort_column_id);

			if mTimeCol#as_column = col#as_column then (
				match filesModel#custom_get_iter path with
				| Some {kind = File f} ->
					ctrl#setFilePosition f ((colX * 10000) / col#width);
					filesModel#custom_row_changed path f.fnode; true
				| _ -> false
			)
			else if mAddCol = col then (
				match filesModel#custom_get_iter path with
				| Some node -> ctrl#addNode node; true
				| None -> false
			)
			else if mSupCol = col then (
				match filesModel#custom_get_iter path with
				| Some node -> ctrl#supNode node; true
				| None -> false
			)
			else (
				if GdkEvent.Button.button ev = 3 then (
					ctrl#setPlayMode AudioFile.Track;
				)
				else (
					if mControlKeyPressed && mShiftKeyPressed then (
						
						if GdkEvent.Button.button ev = 1 then (
							ctrl#setPlayMode AudioFile.Single;
						)
						else if GdkEvent.Button.button ev = 2 then (
							ctrl#setPlayMode AudioFile.Random;
						)
					)
					else (
						if GdkEvent.Button.button ev = 1 then (
							ctrl#setPlayMode AudioFile.Track;
						)
						else if GdkEvent.Button.button ev = 2 then (
							ctrl#setPlayMode AudioFile.Repeat;
						)
					)
				);
				false
			);
		);


	method buttonReleased ev =
		mButtonPressed <- false;
		false


	method keyPressed ev =
		let key = GdkEvent.Key.keyval ev in
		
		if key = GdkKeysyms._Control_L || key = GdkKeysyms._Control_R then
			mControlKeyPressed <- true
		else
		if key = GdkKeysyms._Shift_L || key = GdkKeysyms._Shift_R then
			mShiftKeyPressed <- true
		else
		if key = GdkKeysyms._Alt_L || key = GdkKeysyms._Alt_R then
			mAltKeyPressed <- true;
		
		false

	method keyReleased ev =
		let key = GdkEvent.Key.keyval ev in
		
		if key = GdkKeysyms._Control_L || key = GdkKeysyms._Control_R then
			mControlKeyPressed <- false
		else
		if key = GdkKeysyms._Shift_L || key = GdkKeysyms._Shift_R then
			mShiftKeyPressed <- false
		else
		if key = GdkKeysyms._Alt_L || key = GdkKeysyms._Alt_R then
			mAltKeyPressed <- false;
		
		false


	method rowClicked() = trace"on_filesView_button_release_event";
		if treeView#hover_selection then (
		) else (
			L.iter (fun p -> match filesModel#custom_get_iter p with
				| Some row -> ()
				| None -> traceRed "Path not found !") treeView#selection#get_selected_rows;
		);


	method activeHoverMode path col = trace"on_filesView_row_activated";
		ctrl#play;
		treeView#set_hover_selection true;
		treeView#set_hover_expand true;
		treeView#selection#set_mode`SINGLE;


	method renderState sttRndrr (model:GTree.model) iter =
		let path = model#get_path iter in
		match filesModel#custom_get_iter path with
		| Some node -> (
			match node.state with
			| Off -> sttRndrr#set_properties [`VISIBLE false]
			| Track -> sttRndrr#set_properties [`STOCK_ID "gtk-media-play"(*media-playback-start*); `VISIBLE true]
			| Repeat -> sttRndrr#set_properties [`STOCK_ID "gtk-refresh"(*media-playlist-repeat*); `VISIBLE true]
			| Single -> sttRndrr#set_properties [`STOCK_ID "gtk-media-next"(*media-seek-forward*); `VISIBLE true]
			| Random -> sttRndrr#set_properties [`STOCK_ID "gtk-dialog-question"(*media-seek-forward*); `VISIBLE true]
			| Pause -> sttRndrr#set_properties [`STOCK_ID "gtk-media-pause"(*media-playback-pause*); `VISIBLE true]
			)
		| _ -> sttRndrr#set_properties [`VISIBLE false]


	method renderProgress prgRndrr (model:GTree.model) iter =
		let path = model#get_path iter in
		match filesModel#custom_get_iter path with
		| Some {kind = File f} -> (
			if ctrl#checkPropertys f then (
				prgRndrr#set_properties [
					`VISIBLE true;
					`VALUE f.readPercent;
					`TEXT(Some (AudioFile.progress f))];)
			else
				prgRndrr#set_properties [
					`VISIBLE true;
					`VALUE 0;
					`TEXT(Some "ERROR")];)
(*
			if f.readPercent > 0 then (
				match mTimeCol#widget with
				| Some w -> 
					let drawable = Gdk.Drawable.cast w#as_widget in
					(*let drawable = new GDraw.drawable drawable in*)
					let gc = Gdk.GC.create drawable in
					(*Gdk.GC.set_background gc color
					let drawable = new GDraw.drawable treeView#as_tree_view in
					drawable#set_background (`RGB(200, 180, 160));*)
					trace"drawable";
					let r = treeView#get_cell_area ~path (*~col:mTimeCol*) () in
					let x = Gdk.Rectangle.x r and y = Gdk.Rectangle.y r in
					let w = ((Gdk.Rectangle.width r) * f.readPercent) / 100 in
					let h = Gdk.Rectangle.height r in
	(*				drawable#rectangle x y w h ();*)
					Gdk.Draw.rectangle drawable gc x y w h ();
					traceBlue("render "^soi f.readPercent^" % at x:"^soi x^" y:"^soi y)
				| None -> traceRed"NO mTimeCol#widget";
		*)
		| _ -> prgRndrr#set_properties [`VISIBLE false]


(* observer methods *)
	method notify =	function
		| Ev.StartFile f ->
			let path = filesModel#custom_get_path f.fnode in
			treeView#expand_to_path path;
			treeView#selection#select_path path;
		| Ev.EndFile f ->
			treeView#selection#unselect_path(filesModel#custom_get_path f.fnode)
(*		| Ev.FileList fl -> (
			treeView#set_model None;
			filesModel#setNodes fl;
			treeView#set_model(Some (filesModel :> GTree.model)););
*)
		| Ev.Filter motif -> self#setFilterMotif motif
		| _ -> ()

end
