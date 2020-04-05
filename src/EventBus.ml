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

type notification =
    State of State.t |
    FileChanged of AudioFile.t |
    StartFile of AudioFile.t |
    PauseFile of AudioFile.t |
    EndFile of AudioFile.t |
    EndList of AudioFile.t |
    NewPlaylist of string |
    AddFolder |
    HiddenFilesChanged |
    Filter of string |
    OutputDeviceChanged of string |
    Error of string


let observers : (notification -> unit) list ref = ref []

let addObserver o = observers := o :: !observers

let notify notification =
  List.iter(fun observe -> observe notification) !observers

let asyncNotify notif = GtkThread.async notify notif

