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

module Gst = Gstreamer
module Ev = EventBus

let maxA = 1.

let errorCodeOutputUnderflowed = -9980

let initialize() = Gst.init ()
let terminate() = Gst.deinit ()


class c () = object (self)
	val mutable mFiles : AudioFile.t list = []
	val mutable mState = State.Stop
	val mutable mThread = Thread.self()
	val mPauseLock = Mutex.create()
	val mutable mChangeFiles = false
	val mutable mVolume = maxA /. 2.
	val mutable mOutputDevice = 0
	val mutable mNewOutputDevice = 0


	initializer()
    (*
		try
			mOutputDevice <- Portaudio.get_default_output_device();
			mNewOutputDevice <- mOutputDevice;
		with Portaudio.Error code -> (
(*				Ev.asyncNotify(Ev.Error(Portaudio.string_of_error code));*)
			traceRed(Portaudio.string_of_error code);
  	);
*)

	method getState = mState
(*	method setState s = mState <- s*)

	method isPlaying = mState = State.Play

	method changeFiles files =
		mFiles <- files;
		mChangeFiles <- true;
		if mState <> State.Play && L.length files > 0 then self#play;


	method setVolume volumePercent = mVolume <- (volumePercent *. maxA) /. 100.

	method getOutputDevice = ""
(*		Portaudio.((get_device_info mNewOutputDevice).d_name)*)

	method changeOutputDevice (newOutputDeviceName:string) = ()
  (*
		let open Portaudio in
    let dcount = Portaudio.get_device_count () in

		let rec search id =
			if id < dcount then (
		    let dinfo = Portaudio.get_device_info id in
				
				if dinfo.d_name = newOutputDeviceName then id else search(id + 1)
			)
			else -1
		in
		
		let newOutputDevice = search 0 in
		
		if newOutputDevice >= 0 && newOutputDevice <> mOutputDevice then (
			mNewOutputDevice <- newOutputDevice;
		)
*)

	method getOutputDevices : string list = []
  (*
		let open Portaudio in
    let dcount = Portaudio.get_device_count () in

		let rec search id lst =
			if id < dcount then (
				
        let dinfo = Portaudio.get_device_info id in
				
				if dinfo.d_max_output_channels > 0 then
					search(id + 1) (dinfo.d_name::lst) 
				else
					search(id + 1) lst
			)
			else lst
		in
		L.rev(search 0 [])
*)

	method play = ( match mState with
			| State.Play -> ()
			| State.Pause -> mState <- State.Play; Mutex.unlock mPauseLock
			| State.Stop -> mThread <- Thread.create self#run()
		); Thread.yield()

	method playFile file = self#changeFiles [file]

	method pause = ( match mState with
			| State.Play -> Mutex.lock mPauseLock; mState <- State.Pause
			| State.Pause -> ()
			| State.Stop -> ()
		); Thread.yield()

	method stop = match mState with
			| State.Play -> mState <- State.Stop; Thread.join mThread
			| State.Pause -> mState <- State.Stop; Mutex.unlock mPauseLock; Thread.join mThread
			| State.Stop -> ()


	
	method private run() =
(*		try
*)
		let setState s =
			if mState <> s then (
				mState <- s;
				Ev.asyncNotify(Ev.State mState)
			)
		in

		traceCyan"PLAY";
		
		setState State.Play;

    let timeout = Int64.of_int 100_000_000 in
    let filter = Gst.Message.[End_of_stream; Error] in

		let playChunk file stream =

			if file.newPosition <> file.currentPosition then (
        Gst.Element.seek_simple stream Gst.Format.Time [] file.newPosition;
				file.currentPosition <- file.newPosition;
			);

      match Gst.Bus.(timed_pop_filtered (of_element stream) ~timeout filter) with
      | exception Gst.Timeout -> (
        file.newPosition <- Gst.(Element.position stream Format.Time);
				file.currentPosition <- file.newPosition;

				AudioFile.setReadPercent file
          Int64.(to_int(div(mul file.newPosition (of_int 100)) file.duration));

				Ev.asyncNotify(Ev.FileChanged file);
				true
			)
      | _ -> false
		in
		let rec playFile file stream =

			if mChangeFiles then (
        Gst.Element.(set_state stream State_paused) |> ignore;
        (*AudioFile.close file;*)
				mChangeFiles <- false;
				false
			)
			else (
				match mState with
				| State.Play ->
					if playChunk file stream then
						playFile file stream
					else
						true
				| State.Pause ->
          Gst.Element.(set_state stream State_paused) |> ignore;
          Ev.asyncNotify(Ev.PauseFile file);
          Mutex.(lock mPauseLock; unlock mPauseLock);
          Gst.Element.(set_state stream State_playing) |> ignore;
          Ev.asyncNotify(Ev.StartFile file);
					playFile file stream
				| State.Stop ->
          Gst.Element.(set_state stream State_paused) |> ignore;
          Ev.asyncNotify(Ev.State mState); false
			)
		in
		let rec iterFiles prevFile = function
			| [] -> Ev.asyncNotify(Ev.EndList prevFile);
			| file::tl -> (
        let stream = AudioFile.stream file in
        Gst.Element.(set_state stream State_playing) |> ignore;

        Ev.asyncNotify(Ev.StartFile file);
				
        let continu = playFile file stream in

				Ev.asyncNotify(Ev.EndFile file);
        AudioFile.stop file; 
				
        if continu then iterFiles file tl
				else ()
			)
		in
		let rec loop() =
  		if mState != State.Stop && L.length mFiles > 0 then (
  			loop(iterFiles (L.hd mFiles) mFiles)
  		)
		in
		loop ();
(*
		with e -> Ev.asyncNotify(Ev.Error(Printexc.to_string e));
*)
		setState State.Stop;
		traceCyan"STOP";

end

let make = new c()
