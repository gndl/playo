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
open Sexplib.Std
       
type t = {
  (* Playback volume. Its default value is 50% *)
  mutable volume : float [@default 50.];

  (* Displayed columns *)
  mutable fileListColumns : string list [@default ["Add"; "Name"; "Time"; "Sup"; "Title"; "Artist"; "Album"; "Genre"; "Size"; "Path"]];

  (* Player output device name *)
  mutable outputDevice : string [@default "ALSA audio output"];

  (* Audio files and folders of the library *)
  mutable libraryFiles : string list;

  (* Audio files and folders hidden in the library *)
  mutable libraryHiddenFiles : string list;

  (* List of playlists *)
  mutable playlists : (string * (string * string) list) list;
} [@@deriving sexp]


(* let log_file = open_out "playo.log" *)

let default = {
  volume = 50.;
  fileListColumns = ["Add"; "Name"; "Time"; "Sup"; "Title"; "Artist"; "Album"; "Genre"; "Size"; "Path"];
  outputDevice = "ALSA audio output";
  libraryFiles = [];
  libraryHiddenFiles = [];
  playlists = []
}

let configFileName = (Sys.getenv "HOME")^"/.playo.conf"

let save config = sexp_of_t config |> Sexplib.Sexp.save_hum configFileName

(* Loading of the configuration file *)
let config = match Sexplib.Sexp.load_sexp_conv configFileName t_of_sexp with
  | `Result c -> c
  | `Error _ -> save default; default
  | exception _ -> save default; default

let save() = save config

let getVolume() = config.volume
let setVolume v = config.volume <- v

let getColumns() = config.fileListColumns
let setColumns cs = config.fileListColumns <- cs

let getOutputDevice() = config.outputDevice
let setOutputDevice v = config.outputDevice <- v

let addFiles filenameList = config.libraryFiles <- config.libraryFiles @ filenameList
let removeFile filename = config.libraryFiles <- L.filter ~f:(fun fn -> fn <> filename) config.libraryFiles
let getFiles() = config.libraryFiles

let addHiddenFile filename = config.libraryHiddenFiles <- filename :: config.libraryHiddenFiles
let removeHiddenFile filename = config.libraryHiddenFiles <- L.filter ~f:(fun fn -> fn <> filename) config.libraryHiddenFiles
let getHiddenFiles() = config.libraryHiddenFiles

let setPlaylists pls = config.playlists <- pls
let getPlaylists() = config.playlists
