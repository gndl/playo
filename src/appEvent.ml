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

type notification =
	State of State.t |
	FileChanged of AudioFile.t |
	NodeChanged of AudioFile.node |
	StartFile of AudioFile.t |
	PauseFile of AudioFile.t |
	EndFile of AudioFile.t |
	EndList of AudioFile.t |
	AddFile of AudioFile.node |
	SupFile of AudioFile.node |
	NewPlaylist of string |
	AddFolder |
	Filter of string |
	OutputDeviceChanged of string |
	Error of string

class virtual observer () =	object
	method virtual update : notification -> unit
end


let observers : observer list ref = ref []

let addObserver o = observers := o :: !observers

let notify notif = L.iter(fun o -> o#update notif) !observers

let asyncNotify notif = GtkThread.async notify notif
