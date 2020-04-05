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

type row = AudioFile.node

let unexistentRow = AudioFile.unexistentNode

(* The columns in our custom model *)
let columnList = new GTree.column_list
let nameColumn = columnList#add Gobject.Data.string
let titleColumn = columnList#add Gobject.Data.string
let artistColumn = columnList#add Gobject.Data.string
let albumColumn = columnList#add Gobject.Data.string
let genreColumn = columnList#add Gobject.Data.string
let timeColumn = columnList#add Gobject.Data.string
let readPercentColumn = columnList#add Gobject.Data.int
let sizeColumn = columnList#add Gobject.Data.string
let pathColumn = columnList#add Gobject.Data.string


(** The custom model *)
class c = object (self)
  inherit [row,row,unit,unit] GTree.custom_tree_model columnList

  val mutable mRootDirs : row array = [||]

  method! custom_flags : GtkEnums.tree_model_flags list = [`ITERS_PERSIST]

  method custom_get_iter (path:Gtk.tree_path) : row option =
    let indices  = GTree.Path.get_indices path in

    let rec searchVisibleRow ri vi rws =

      if ri >= A.length rws then rws.(ri - 1)
      else (
  	let rw = rws.(ri)
  	in

  	if rw.visible then (
    	  if vi = 0 then rw
    	  else
  	    searchVisibleRow (ri + 1) (vi - 1) rws
  	) else
  	  searchVisibleRow (ri + 1) vi rws
      )
    in

    let rec go ii rws =

      if ii >= A.length indices then None
      else (
	let visibleRowIndex = indices.(ii) in

	if visibleRowIndex < 0 || visibleRowIndex >= A.length rws then
	  None
	else (
	  let rw = searchVisibleRow 0 visibleRowIndex rws
	  in
	  if ii = A.length indices - 1 then
	    Some rw
	  else match rw.kind with
	    | Dir d -> go (ii + 1) d.children
	    | _ -> None
	)
      )
    in
    go 0 mRootDirs


  method custom_get_path (row:row) : Gtk.tree_path =
    let rec mkl rw l = match rw.parent with
      | None -> rw.idx::l
      | Some pd -> mkl pd.dnode (rw.idx::l)
    in
    GTree.Path.create(mkl row [])


  method custom_value (_:Gobject.g_type) (row:row) ~column =
    if column = 0 then `STRING(Some row.name) else
    if column = 5 then `STRING(Some row.time) else
    if column = 7 then `STRING(Some row.size) else
    if column = 8 then `STRING(Some row.path)
    else (
      match row.kind with
      | File f -> (
	  if column = 1 then `STRING(Some f.title) else
	  if column = 2 then `STRING(Some f.artist) else
	  if column = 3 then `STRING(Some f.album) else
	  if column = 4 then `STRING(Some f.genre) else
	  if column = 6 then `INT f.readPercent else
	    assert false
	)
      | _ -> if column = 6 then `INT 0 else `STRING None
    )


  method custom_iter_next (row:row) : row option =
    let nidx = row.idx + 1 in
    match row.parent with
    | None -> if nidx < A.length mRootDirs then Some mRootDirs.(nidx)
      else None
    | Some pd -> if nidx < A.length pd.children then Some pd.children.(nidx)
      else None


  method custom_iter_children (rowopt:row option) : row option =
    match rowopt with
    | Some {kind = Dir { children = nodes; _ }; _} -> Some nodes.(0)
    | _ -> None


  method custom_iter_has_child (row:row) : bool =
    match row.kind with 
    | Dir { children = nodes; _} -> A.length nodes > 0
    | _ -> false


  method custom_iter_n_children (rowopt:row option) : int =
    match rowopt with
    | None -> A.length mRootDirs
    | Some {kind = Dir { children = nodes; _}; _} -> A.length nodes
    | _ -> 0


  method custom_iter_nth_child (rowopt:row option) (n:int) : row option =
    match rowopt with
    | None when A.length mRootDirs > 0 -> Some mRootDirs.(0)
    | Some {kind = Dir { children = ns; _}; _} when n < A.length ns -> Some ns.(n)
    | _ -> None


  method custom_iter_parent (row:row) : row option =
    match row.parent with Some pd -> Some pd.dnode | None -> None

  method custom_encode_iter cr = cr, (), ()
  method custom_decode_iter cr () () = cr


  initializer
    Ev.addObserver self#observe;

  method fileInserted node =
    self#custom_row_inserted (self#custom_get_path node) node

  method fileDeleted node =
    self#custom_row_deleted (self#custom_get_path node)
      
  method setNodes nodes = mRootDirs <- nodes

  method updateNode node =
    self#custom_row_changed (self#custom_get_path node) node

  method filter regexp =

    let rec fltr visible row =
      row.visible <- (match row.kind with
	  | Dir dir -> A.fold_left ~f:fltr ~init:false dir.children
		       || (try ignore(Str.search_forward regexp (S.lowercase_ascii row.name) 0);
			     true
  			   with Not_found -> false)
	  | File f -> (try
  			 ignore(Str.search_forward regexp (S.lowercase_ascii f.title) 0);
			 true
  		       with Not_found -> try
  			   ignore(Str.search_forward regexp (S.lowercase_ascii f.artist) 0);
  			   true
  			 with Not_found -> try
  			     ignore(Str.search_forward regexp (S.lowercase_ascii f.album) 0);
  			     true
  			   with Not_found -> try
  			       ignore(Str.search_forward regexp (S.lowercase_ascii row.name) 0);
  			       true
  			     with Not_found -> try
  				 ignore(Str.search_forward regexp (S.lowercase_ascii f.genre) 0);
  				 true
  			       with Not_found -> false)
	  | Null -> false);

      visible || row.visible
    in
    ignore(A.fold_left ~f:fltr ~init:false mRootDirs)


  (* observer methods *)
  method observe =	 function
    | Ev.FileChanged file -> self#updateNode file.fnode
    | _ -> ()

end

