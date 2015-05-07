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

let maxA = 1.

module Ev = AppEvent

let initialize() = Portaudio.init ()
let terminate() = Portaudio.terminate ()


class c () = object (self)
	val mutable mDevice = 5
	val mutable mFiles : AudioFile.t list = []
	val mutable mState = State.Stop
	val mutable mThread = Thread.self()
	val mPauseLock = Mutex.create()
	val mutable mChangeFiles = false
	val mutable mVolume = maxA /. 2.


	initializer

(*		mDevice <- Portaudio.get_default_output_device();
*)		Ev.addObserver (self :> Ev.observer);


	method getState = mState
(*	method setState s = mState <- s*)

	method private threadSetState s =
		if mState <> s then (mState <- s; Ev.notify(Ev.State mState))

	method isPlaying = mState = State.Play
(*
	method changeFile file =
		if mFiles != file then (
			match file.voice with
				| None -> ()
				| Some v -> (
					mFiles <- file;
					mChangeFiles <- true;
					Ev.notify(CurrentFile mFiles);
			);
		); Thread.yield()
*)
	method changeFiles files =
		mFiles <- files;
		mChangeFiles <- true;
		if mState <> State.Play then self#play;


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

(* observer methods *)
	method notify =	function
		| Ev.Volume volumePercent -> mVolume <- (volumePercent *. maxA) /. 100.
		| _ -> ()

	
	method private run() =(* *)traceCyan"PLAY";
		self#threadSetState State.Play;

		let inBufLen = 10080 in

		let makeOutputStream file =
			
  		let rate = foi(AudioFile.rate file) in
			let channels = AudioFile.channels file in
			let bufframes = inBufLen / channels in
			
  		let outparam = Portaudio.{
  			channels;
				device = mDevice;
  			sample_format = format_float32;
				latency = 1.
				}
  		in
			trace((AudioFile.name file)^" : channels = "^soi channels^", rate = "^sof rate^", bufframes = "^soi bufframes);
  		let stream = Portaudio.open_stream None (Some outparam) ~interleaved:true rate bufframes []
			in
			Portaudio.start_stream stream;
			stream
		in

		let defOutputStream file prevFile = function
			| None -> makeOutputStream file
			| Some stream ->
				if file == prevFile
					|| (AudioFile.rate file) <> (AudioFile.rate prevFile)
					|| (AudioFile.channels file) <> (AudioFile.channels prevFile)
				then (
					Portaudio.close_stream stream;
					makeOutputStream file
				)
				else stream
		in

		let open Bigarray in

		let inputBuffer = A.make inBufLen 0. in
(*		let outputBuffer = Genarray.create float32 c_layout [|inBufLen|] in*)
		let outputBuffer = Array1.create float32 c_layout inBufLen in
		let genOutputBuffer = genarray_of_array1 outputBuffer in
		
		let playChunk file outputStream =

			let channels = AudioFile.channels file in
			let stream = AudioFile.stream file in
			let readcount = Sndfile.read stream inputBuffer in

			if readcount > 0 then (
				
				for i = 0 to readcount - 1 do
					outputBuffer.{i} <- (inputBuffer.(i) *. mVolume);
					(*Genarray.set outputBuffer [|i|] (inputBuffer.(i) *. mVolume);*)
				done;

        Portaudio.write_stream_ba outputStream genOutputBuffer 0 (readcount / channels);
				
				if file.newFrame = file.curFrame then (
					let framesRead = Int64.of_int(readcount / channels) in
					file.newFrame <- Int64.add file.curFrame framesRead;
				)
				else (
(*					let offset = Int64.sub file.newFrame file.curFrame in
					file.newFrame <- Sndfile.seek stream offset Sndfile.SEEK_CUR;*)
					ignore(Sndfile.seek stream file.newFrame Sndfile.SEEK_SET);
				);
				
				AudioFile.setReadPercent file (Int64.to_int(Int64.div(Int64.mul file.newFrame
					(Int64.of_int 100)) (Sndfile.frames stream)));
					
				Ev.notify(Ev.FileChanged file);
				true
			)
			else (
				ignore(Sndfile.seek stream Int64.zero Sndfile.SEEK_SET);
				file.newFrame <- Int64.zero;
				file.curFrame <- Int64.zero;
				(*file.readPercent <- 0;
				Ev.notify(FileChanged file);*)
				false)
		in
		let rec playFile file outputStream =
	
			if mChangeFiles then (
				mChangeFiles <- false;
				false
			)
			else (
				match mState with
				| State.Play ->
					if playChunk file outputStream then
						playFile file outputStream
					else true
				| State.Pause -> Ev.notify(Ev.PauseFile file); Mutex.lock mPauseLock;
					Mutex.unlock mPauseLock; Ev.notify(Ev.StartFile file);
					playFile file outputStream
				| State.Stop -> Ev.notify(Ev.State mState); false
			)
		in
		let rec iterFiles prevFile outputStreamOpt = function
			| [] -> Ev.notify(Ev.EndList prevFile); outputStreamOpt
			| file::tl -> (
  			Ev.notify(Ev.StartFile file);
  			match file.voice with
  			| None -> Ev.notify(Ev.EndFile file); outputStreamOpt
  			| Some talker -> (
  				
					let os = defOutputStream file prevFile outputStreamOpt in

					if file.newFrame <> file.curFrame then (
  					ignore(Sndfile.seek talker.stream file.newFrame Sndfile.SEEK_SET);
  					file.curFrame <- file.newFrame;
  				);
  				
					let continu = playFile file os in
					
					Ev.notify(Ev.EndFile file);
					
					if continu then iterFiles file (Some os) tl
					else (Some os)
				)
			)
		in
		let rec loop outputStreamOpt =
  		if mState != State.Stop && L.length mFiles > 0 then (
  			loop(iterFiles (L.hd mFiles) outputStreamOpt mFiles)
  		)
			else match outputStreamOpt with
			| None -> ()
			| Some stream -> Portaudio.close_stream stream
		in
		loop None;

		self#threadSetState State.Stop;
		traceCyan"STOP";

		(*with x -> Ev.notify(Error "faile to open device");*)

end

let make = new c()
(*
let run filename =
	try

	let dev = Ao.open_live() in

	let file = Sndfile.openfile filename in
	let nbChs = Sndfile.channels file in
	let outBufLen = (2*3*4*5*6)* 4 * 8 in
	let inBufLen = outBufLen * nbChs / 4 in

	let inBuf = A.make inBufLen 0.0 in
	let outBuf = S.create outBufLen in

	let rec play readcount =

		if readcount > 0 then (

			let rec write ip op =
				let r = iof (inBuf.(ip) *. maxA) in
				let l = iof (inBuf.(ip + 1) *. maxA) in
				let rpfb = coi (r land 0xff) in
				let rpfr = coi ((r lsr 8) land 0xff) in
				let lpfb = coi (l land 0xff) in
				let lpfr = coi ((l lsr 8) land 0xff) in

				outBuf.[op] <- rpfb;
				outBuf.[op + 1] <- rpfr;
				outBuf.[op + 2] <- lpfb;
				outBuf.[op + 3] <- lpfr;

				if ip < readcount - nbChs then write (ip + nbChs) (op + 4)
			in
			write 0 0;

			Ao.play dev outBuf;
			play(Sndfile.read file inBuf);
		);
	in
	play(Sndfile.read file inBuf);

	Sndfile.close file;
	Ao.close dev;

	with x -> Printf.printf "faile to open device"
*)
