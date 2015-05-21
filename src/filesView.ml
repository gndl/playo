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

let getIndex elem list =
	let rec loop i = function
		| [] -> -1
		| e::tl -> if e = elem then i else loop(i + 1) tl
	in
	loop 0 list

let addColName = "Add"
let nameColName = "Name"
let titleColName = "Title"
let artistColName = "Artist"
let albumColName = "Album"
let genreColName = "Genre"
let timeColName = "Time"
let sizeColName = "Size"
let pathColName = "Path"
let supColName = "Sup"

let columns = [addColName; nameColName; titleColName; artistColName; albumColName; genreColName; timeColName; sizeColName; pathColName; supColName]


class c (filesModel:FilesModel.c) (filesTreeview:GTree.view) (ctrl:Controler.c) toplevel =
	let nullColumn = GTree.view_column() in
	object (self)
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
		Ev.addObserver self#observe;

		filesTreeview#set_model(Some (filteredFilesModel :> GTree.model));
		filesTreeview#set_headers_clickable true;
		filesTreeview#set_reorderable true;
		filesTreeview#selection#set_mode`MULTIPLE;

		ignore(filesTreeview#event#connect#button_press ~callback:self#buttonPressed);
		ignore(filesTreeview#event#connect#button_release ~callback:self#buttonReleased);
		ignore(filesTreeview#connect#row_activated ~callback:self#activeHoverMode);
	  ignore(filesTreeview#connect#cursor_changed ~callback:self#changeFiles);

		let txtRndrr = GTree.cell_renderer_text [] in

		let pixRndrr = GTree.cell_renderer_pixbuf [`STOCK_ID "gtk-add"] in

		let addColumn columnName =
			
			if columnName = addColName then (
    		mAddCol <- GTree.view_column ~renderer:(pixRndrr,[]) ();
				mAddCol#set_sort_column_id(getIndex columnName columns);
    		ignore(filesTreeview#append_column mAddCol);
			)
			else if columnName = nameColName then (
    		let sttRndrr = GTree.cell_renderer_pixbuf [`STOCK_ID "gtk-media-play"; `VISIBLE false] in
    		mNameCol <- GTree.view_column ~title:columnName();
    		mNameCol#pack sttRndrr;
    		mNameCol#set_cell_data_func sttRndrr (self#renderState sttRndrr);
    		mNameCol#pack txtRndrr;
    		mNameCol#add_attribute txtRndrr "text" nameColumn;
    		mNameCol#set_resizable true;
				mNameCol#set_sort_column_id(getIndex columnName columns);
				ignore(filesTreeview#append_column mNameCol);
    		filesTreeview#set_expander_column (Some mNameCol);
			)
			else if columnName = titleColName then (
    		mTitleCol <- GTree.view_column ~title:columnName ~renderer:(txtRndrr,["text", titleColumn]) ();
    		mTitleCol#set_resizable true;
				mTitleCol#set_sort_column_id(getIndex columnName columns);
				ignore(filesTreeview#append_column mTitleCol);
			)
			else if columnName = artistColName then (
    		mArtistCol <- GTree.view_column ~title:columnName ~renderer:(txtRndrr,["text", artistColumn]) ();
    		mArtistCol#set_resizable true;
				mArtistCol#set_sort_column_id(getIndex columnName columns);
				ignore(filesTreeview#append_column mArtistCol);
			)
			else if columnName = albumColName then (
    		mAlbumCol <- GTree.view_column ~title:columnName ~renderer:(txtRndrr,["text", albumColumn]) ();
    		mAlbumCol#set_resizable true;
				mAlbumCol#set_sort_column_id(getIndex columnName columns);
				ignore(filesTreeview#append_column mAlbumCol);
			)
			else if columnName = genreColName then (
    		mGenreCol <- GTree.view_column ~title:columnName ~renderer:(txtRndrr,["text", genreColumn]) ();
    		mGenreCol#set_resizable true;
				mGenreCol#set_sort_column_id(getIndex columnName columns);
				ignore(filesTreeview#append_column mGenreCol);
			)
			else if columnName = timeColName then (
    		let prgRndrr = GTree.cell_renderer_progress [] in
    		mTimeCol <- GTree.view_column ~title:columnName ~renderer:(prgRndrr,["text", timeColumn]) ();
    (*	col#add_attribute rndrr "value" readPercentColumn;*)
    		mTimeCol#set_cell_data_func prgRndrr (self#renderProgress prgRndrr);
    		mTimeCol#set_resizable true;
(*				col#set_sizing `AUTOSIZE;*)
				mTimeCol#set_sort_column_id(getIndex columnName columns);
				ignore(filesTreeview#append_column mTimeCol);
			)
			else if columnName = sizeColName then (
    		mSizeCol <- GTree.view_column ~title:columnName ~renderer:(txtRndrr,["text", sizeColumn]) ();
    		mSizeCol#set_resizable true;
				mSizeCol#set_sort_column_id(getIndex columnName columns);
				ignore(filesTreeview#append_column mSizeCol);
			)
			else if columnName = pathColName then (
    		mPathCol <- GTree.view_column ~title:columnName ~renderer:(txtRndrr,["text", pathColumn]) ();
    		mPathCol#set_resizable true;
				mPathCol#set_sort_column_id(getIndex columnName columns);
				ignore(filesTreeview#append_column mPathCol);
			)
			else if columnName = supColName then (
    		let pixRndrr = GTree.cell_renderer_pixbuf [`STOCK_ID "gtk-close"] in
    		mSupCol <- GTree.view_column ~renderer:(pixRndrr,[]) ();
				mSupCol#set_sort_column_id(getIndex columnName columns);
    		ignore(filesTreeview#append_column mSupCol);
			)
		in
		
		L.iter addColumn (match Configuration.getColumns with
			| [] -> Configuration.setColumns columns; columns
			| l -> l);

(*
  filesTreeview#selection#connect#after#changed ~callback:
    (fun () -> traceBlue "selection changed";
(*      L.iter filesTreeview#selection#get_selected_rows ~f:
        (fun p -> match filesModel#custom_get_iter p with
				| Some row -> trace("file "^row.nd.name^" selected");(* ctrl#changeFile row.nd*)
				| None -> trace "Path not found !");
*)		);

	filesTreeview#connect#after#row_activated ~callback:
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


	method init() = ()
	
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

	method changeFiles() =
		
		if filesTreeview#hover_selection && mButtonPressed then (
			filesTreeview#set_hover_selection false;
			filesTreeview#set_hover_expand false;
			filesTreeview#selection#set_mode`MULTIPLE;
		)
		else (
			let rowList = ListLabels.fold_left ~init:[]
				~f:(fun rl path -> match filesModel#custom_get_iter path with
					| Some r -> trace("row "^r.name^" selected"); r::rl
					| None -> traceRed "Path not found !"; rl)
				filesTreeview#selection#get_selected_rows
			in
			ctrl#changeFiles rowList;
		)


	method buttonPressed ev =
		mButtonPressed <- true;
		let x = iof (GdkEvent.Button.x ev) in
		let y = iof (GdkEvent.Button.y ev) in

		match filesTreeview#get_path_at_pos ~x ~y with
		| None -> false
		| Some (path, col, colX, colY) -> (
			let colId = col#get_sort_column_id in
			
			if mTimeCol#get_sort_column_id = colId then (
				match filesModel#custom_get_iter path with
				| Some {kind = File f} ->
					ctrl#setFilePosition f ((colX * 10000) / col#width);
					filesModel#custom_row_changed path f.fnode; true
				| _ -> false
			)
			else if mAddCol#get_sort_column_id = colId then (
				match filesModel#custom_get_iter path with
				| Some node -> ctrl#addNode node; true
				| None -> false
			)
			else if mSupCol#get_sort_column_id = colId then (
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


	method rowClicked() =
		if filesTreeview#hover_selection then ()
		else (
			L.iter (fun p -> match filesModel#custom_get_iter p with
				| Some row -> ()
				| None -> traceRed "Path not found !") filesTreeview#selection#get_selected_rows;
		);


	method activeHoverMode path col =
		ctrl#play;
		filesTreeview#set_hover_selection true;
		filesTreeview#set_hover_expand true;
		filesTreeview#selection#set_mode`SINGLE;


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
					let drawable = new GDraw.drawable filesTreeview#as_tree_view in
					drawable#set_background (`RGB(200, 180, 160));*)
					trace"drawable";
					let r = filesTreeview#get_cell_area ~path (*~col:mTimeCol*) () in
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
	method observe =	function
		| Ev.StartFile f ->
			let path = filesModel#custom_get_path f.fnode in
			filesTreeview#expand_to_path path;
			filesTreeview#selection#select_path path;
		| Ev.EndFile f ->
			filesTreeview#selection#unselect_path(filesModel#custom_get_path f.fnode)
(*		| Ev.FileList fl -> (
			filesTreeview#set_model None;
			filesModel#setNodes fl;
			filesTreeview#set_model(Some (filesModel :> GTree.model)););
*)
		| Ev.Filter motif -> self#setFilterMotif motif
		| _ -> ()

end
