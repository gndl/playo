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
open Config_file

let group = new group
let configFileName = (Sys.getenv "HOME")^"/.playo.conf"

(* Player volume. Its default value is 50% *)
let volume = new float_cp ~group ["player"; "volume"] 50. "Player volume (%)"

(* Displayed columns *)
let columns = new list_cp string_wrappers ~group ["player"; "columns"] [] "Displayed columns"

(* Player output device. Its default value is empty *)
let outputDevice = new string_cp ~group ["player"; "outputDevice"] "" "Player output device"

let files = new list_cp string_wrappers ~group ["library"; "files"] [] "Audio files and folders of the library"
let excluded = new list_cp string_wrappers ~group ["library"; "excluded"] [] "Audio files and folders excluded from the library"

let playlists = new list_cp (tuple2_wrappers string_wrappers (
	list_wrappers (tuple2_wrappers string_wrappers string_wrappers)))
	~group ["playlists"] [] "List of playlists"


(* Loading of the configuration file *)

let log_file = open_out "playo.log";;
group#read
  ~on_type_error:
  (fun groupable_cp raw_cp output filename in_channel ->
     Printf.fprintf log_file
       "Type error while loading configuration parameter %s from file %s.\n%!"
       (S.concat "." groupable_cp#get_name) filename;
     output log_file; (* get more information into log_file *)
  )
	configFileName


let getVolume = volume#get
let setVolume v = volume#set v

let getColumns = columns#get
let setColumns cs = columns#set cs

let getOutputDevice = outputDevice#get
let setOutputDevice v = outputDevice#set v


let addFiles filenameList = files#set(files#get @ filenameList)
let removeFile filename = files#set(L.filter(fun fn -> fn <> filename) files#get)
let getFiles = files#get

let addExcludedFile filename = excluded#set(filename :: excluded#get)
let removeExcludedFile filename = excluded#set(L.filter(fun fn -> fn <> filename) excluded#get)
let getExcludedFiles = excluded#get

let setPlaylists pls = playlists#set pls
let getPlaylists = playlists#get

let save() = group#write configFileName
