(* 
 * Copyright (C) 2015 Ga�tan Dubreil
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

module Ev = AppEvent

let maxA = 1.

let errorCodeOutputUnderflowed = -9980

let initialize() = Portaudio.init ()
let terminate() = Portaudio.terminate ()


class c () = object (self)
	val mutable mFiles : AudioFile.t list = []
	val mutable mState = State.Stop
	val mutable mThread = Thread.self()
	val mPauseLock = Mutex.create()
	val mutable mChangeFiles = false
	val mutable mVolume = maxA /. 2.
	val mutable mOutputDevice = 0
	val mutable mNewOutputDevice = 0


	initializer
		try
			mOutputDevice <- Portaudio.get_default_output_device();
			mNewOutputDevice <- mOutputDevice;
		with Portaudio.Error code -> (
(*				Ev.asyncNotify(Ev.Error(Portaudio.string_of_error code));*)
			traceRed(Portaudio.string_of_error code);
  	);


	method getState = mState
(*	method setState s = mState <- s*)

	method isPlaying = mState = State.Play

	method changeFiles files =
		mFiles <- files;
		mChangeFiles <- true;
		if mState <> State.Play && L.length files > 0 then self#play;


	method setVolume volumePercent = mVolume <- (volumePercent *. maxA) /. 100.

	method getOutputDevice =
		Portaudio.((get_device_info mNewOutputDevice).d_name)

	method changeOutputDevice newOutputDeviceName =
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


	method getOutputDevices =
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

		let inBufLen = 10080 in

		let rec makeOutputStream file device =
			try
  		let rate = foi(AudioFile.rate file) in
			let channels = AudioFile.channels file in
			let bufframes = inBufLen / channels in

  		let outparam = Portaudio.{
  			channels;
				device;
  			sample_format = format_float32;
				latency = 1.
				}
  		in
			trace((AudioFile.name file)^" : channels = "^soi channels^", rate = "^sof rate^", bufframes = "^soi bufframes);
  		let stream = Portaudio.open_stream None (Some outparam) ~interleaved:true rate bufframes []
			in
			Portaudio.start_stream stream;
			
			if device <> mOutputDevice || device <> mNewOutputDevice then (
				mOutputDevice <- device;
				mNewOutputDevice <- device;
				Ev.asyncNotify(Ev.OutputDeviceChanged self#getOutputDevice);
			);
			
			stream
			with Portaudio.Error code -> (
				Ev.asyncNotify(Ev.Error(Portaudio.string_of_error code));

				(* If the new device raise an error, we fallback to the previous device *)
				if device <> mOutputDevice then
					makeOutputStream file mOutputDevice
				else
					raise (Portaudio.Error code)
			)
		in

		let defOutputStream file prevFile = function
			| None -> makeOutputStream file mNewOutputDevice
			| Some stream ->
				if file == prevFile || mNewOutputDevice <> mOutputDevice
					|| (AudioFile.rate file) <> (AudioFile.rate prevFile)
					|| (AudioFile.channels file) <> (AudioFile.channels prevFile)
				then (
					Portaudio.close_stream stream;
					makeOutputStream file mNewOutputDevice
				)
				else stream
		in

		let open Bigarray in

		let inputBuffer = A.make inBufLen 0. in
		let outputBuffer = Array1.create float32 c_layout inBufLen in
		let genOutputBuffer = genarray_of_array1 outputBuffer in
		
		let playChunk file outputStream =
			try
			let channels = AudioFile.channels file in
			let stream = AudioFile.stream file in
			let readcount = Sndfile.read stream inputBuffer in

			if readcount > 0 then (
				
				for i = 0 to readcount - 1 do
					outputBuffer.{i} <- (inputBuffer.(i) *. mVolume);
				done;

(* *)				
       	Portaudio.write_stream_ba outputStream genOutputBuffer 0
																		(readcount / channels);
(**)
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
					
				Ev.asyncNotify(Ev.FileChanged file);
				true
			)
			else (
				ignore(Sndfile.seek stream Int64.zero Sndfile.SEEK_SET);
				file.newFrame <- Int64.zero;
				file.curFrame <- Int64.zero;
				(*file.readPercent <- 0;
				Ev.asyncNotify(FileChanged file);*)
				false
			)
			with Portaudio.Error code -> (
				let msg = "Portaudio.write_stream_ba Error code "^soi code^" : "^Portaudio.string_of_error code
				in
				if code = errorCodeOutputUnderflowed then (
					traceYellow msg;
					true
				)
				else (
					Ev.asyncNotify(Ev.Error msg);
					false
				)
			)
		in
		let rec playFile file outputStream =
	
			if mChangeFiles then (
				mChangeFiles <- false;
				(false, outputStream)
			)
			else (
				let outputStream =
					if mNewOutputDevice <> mOutputDevice then (
						defOutputStream file file (Some outputStream)
					)
					else outputStream
				in
				match mState with
				| State.Play ->
					if playChunk file outputStream then
						playFile file outputStream
					else
						(true, outputStream)
				| State.Pause -> Ev.asyncNotify(Ev.PauseFile file); Mutex.lock mPauseLock;
					Mutex.unlock mPauseLock; Ev.asyncNotify(Ev.StartFile file);
					playFile file outputStream
				| State.Stop -> Ev.asyncNotify(Ev.State mState); (false, outputStream)
			)
		in
		let rec iterFiles prevFile outputStreamOpt = function
			| [] -> Ev.asyncNotify(Ev.EndList prevFile); outputStreamOpt
			| file::tl -> (
  			Ev.asyncNotify(Ev.StartFile file);
  			match file.voice with
  			| None -> Ev.asyncNotify(Ev.EndFile file); outputStreamOpt
  			| Some talker -> (
  				
					let os = defOutputStream file prevFile outputStreamOpt in

					if file.newFrame <> file.curFrame then (
  					ignore(Sndfile.seek talker.stream file.newFrame Sndfile.SEEK_SET);
  					file.curFrame <- file.newFrame;
  				);
  				
					let (continu, outputStream) = playFile file os in
					
					Ev.asyncNotify(Ev.EndFile file);
					
					if continu then iterFiles file (Some outputStream) tl
					else (Some outputStream)
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
(*
		with e -> Ev.asyncNotify(Ev.Error(Printexc.to_string e));
*)
		setState State.Stop;
		traceCyan"STOP";

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
