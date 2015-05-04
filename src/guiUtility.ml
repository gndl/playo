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

let all_files () =
  let f = GFile.filter ~name:"All" () in
  f#add_pattern "*" ;
  f

let is_string_prefix s1 s2 =
  let l1 = S.length s1 in
  let l2 = S.length s2 in
  l1 <= l2 && s1 = S.sub s2 0 l1

let image_filter () =
  let f = GFile.filter ~name:"Images" () in
  f#add_custom [ `MIME_TYPE ]
    (fun info ->
      let mime = L.assoc `MIME_TYPE info in
      is_string_prefix "image/" mime) ;
  f

let text_filter () = 
  GFile.filter 
    ~name:"Caml source code" 
    ~patterns:[ "*.ml"; "*.mli"; "*.mly"; "*.mll" ] ()

let sessionFilter () = 
  GFile.filter 
    ~name:"Session" 
    ~patterns:[ "*.es"] ()

let audioFilter () = 
  GFile.filter 
    ~name:"Audio" 
    ~patterns:AudioFile.audio_ext_pattern ()


let getEntry parent title defaultValue = 
	let dialog = GWindow.dialog ~title
		~parent ~destroy_with_parent:true ~show:true () in
	let entry = GEdit.entry ~text:defaultValue ~packing: dialog#vbox#add () in

	dialog#add_button_stock `OK `OK ;
  dialog#add_button_stock `CANCEL `CANCEL ;

  let res = match dialog#run () with
	  | `OK -> Some entry#text
	  | `DELETE_EVENT | `CANCEL -> None
	in
	dialog#destroy (); res


let showErrorMessage parent message = 
  let dialog = GWindow.message_dialog ~message
      ~message_type:`ERROR 
      ~buttons:GWindow.Buttons.close 
      ~parent ~destroy_with_parent:true ~show:true () in
  ignore (dialog#run ()) ;
  dialog#destroy ()


let selectFile parent filter =
  let dialog = GWindow.file_chooser_dialog 
      ~action:`SELECT_FOLDER
(*      ~title:"Open File"*)
      ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
	dialog#set_select_multiple true;
  dialog#add_filter (filter ()) ;
  let res = match dialog#run () with
	  | `OPEN -> Some dialog#get_filenames
	  | `DELETE_EVENT | `CANCEL -> None
	in
  dialog#destroy (); res


let saveFile parent filter =
  let dialog = GWindow.file_chooser_dialog 
      ~action:`SAVE
(*      ~title:"Open File"*)
      ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `SAVE `SAVE ;
  dialog#add_filter (filter ()) ;
  let res = match dialog#run () with
	  | `SAVE -> dialog#filename
	  | `DELETE_EVENT | `CANCEL -> None
	in
  dialog#destroy (); res
