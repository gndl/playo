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
open FFmpeg
open Avutil

module Ev = EventBus

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
        Ev.asyncNotify(Ev.Error(Portaudio.string_of_error code));
	traceRed("Player.initializer "^Portaudio.string_of_error code);
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

    let setState s =
      if mState <> s then (
	mState <- s;
	Ev.asyncNotify(Ev.State mState)
      )
    in

    traceCyan"PLAY";

    setState State.Play;

    let bufLen = 16384 in

    let rec makeOutputStream file device =
      try
  	let rate = foi(AudioFile.rate file) in
	let channels = AudioFile.channels file in
        let bufframes = bufLen / channels in

  	let outparam = Portaudio.{
  	    channels;
	    device;
  	    sample_format = format_float32;
	    latency = 1.
	  }
  	in
	log[AudioFile.name file;" : channels = ";soi channels;", rate = ";sof rate;", bufframes = ";soi bufframes;", device = ";soi device];
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
	if mNewOutputDevice <> mOutputDevice
	|| (AudioFile.rate file) <> (AudioFile.rate prevFile)
	|| (AudioFile.channels file) <> (AudioFile.channels prevFile)
	then (
	  Portaudio.close_stream stream;
	  makeOutputStream file mNewOutputDevice
	)
	else stream
    in

    let open Bigarray in
    (*
    let outBuf = Array1.create float32 c_layout bufLen in
    let genOutBuf = genarray_of_array1 outBuf in
    let outBufPos = ref 0 in
  let sin = Array.init bufLen (fun t -> sin(float_of_int t *. 2. *. pi *. 110. /. 44100.)) in
for i = 0 to bufLen - 1 do
outBuf.{i} <- sin.(i) *. mVolume;
          done;
*)
    let playChunk file outStream =

      let channels = AudioFile.channels file in
      let talker = AudioFile.talker file in

      match Av.read_audio talker.stream with
      | Av.Audio af -> (
          let buffer = Resampler.convert talker.resampler af in
          let readCount = Array1.dim buffer in
          let samplesPerChannel = readCount / channels in

	  for i = 0 to readCount - 1 do
	    buffer.{i} <- buffer.{i} *. mVolume;
          done;

          let genOutBuf = genarray_of_array1 buffer in

          let continu = try
              (*
              if !outBufPos + readCount > bufLen then (
                let samplesPerChannel = !outBufPos / channels in
                outBufPos := 0;*)

       	      Portaudio.write_stream_ba outStream genOutBuf 0 samplesPerChannel;

              AudioFile.addToPosition file samplesPerChannel;

	      Ev.asyncNotify(Ev.FileChanged file);
                (*
              );
*)
              true
            with Portaudio.Error code -> (
	        if code = errorCodeOutputUnderflowed then (
                  traceYellow "Portaudio.write_stream_ba output underflowed error code"; true
                )
	        else (
	          let msg = "Portaudio.write_stream_ba Error code "^soi code^" : "^Portaudio.string_of_error code
	          in
	          Ev.asyncNotify(Ev.Error msg); false
	        )
	      )
          in
(*
for i = 0 to readCount - 1 do
outBuf.{!outBufPos + i} <- buffer.{i} *. mVolume;
          done;
          outBufPos := !outBufPos + readCount;
          *)
	  continu
	)
      | Av.End_of_file -> (
          AudioFile.resetPosition file;
	  false
	)
      | exception Avutil.Failure msg -> Ev.asyncNotify(Ev.Error msg); false
    in
    let rec playFile file outStream =

      if mChangeFiles then (
	mChangeFiles <- false;
	(false, outStream)
      )
      else (
	let outStream =
          if mNewOutputDevice <> mOutputDevice then (
	    defOutputStream file file (Some outStream)
	  )
	  else outStream
	in
	match mState with
	| State.Play ->
	  if playChunk file outStream then
	    playFile file outStream
	  else
	    (true, outStream)
	| State.Pause -> Ev.asyncNotify(Ev.PauseFile file); Mutex.lock mPauseLock;
	  Mutex.unlock mPauseLock; Ev.asyncNotify(Ev.StartFile file);
	  playFile file outStream
	| State.Stop -> Ev.asyncNotify(Ev.State mState); (false, outStream)
      )
    in
    let rec iterFiles prevFile outStreamOpt = function
      | [] -> Ev.asyncNotify(Ev.EndList prevFile); outStreamOpt
      | file::tl -> (
  	  Ev.asyncNotify(Ev.StartFile file);
  	  match file.voice with
  	  | None -> Ev.asyncNotify(Ev.EndFile file); outStreamOpt
  	  | Some talker -> (
	      let os = defOutputStream file prevFile outStreamOpt in

              AudioFile.checkSeek file |> ignore;

	      let (continu, outStream) = playFile file os in

	      Ev.asyncNotify(Ev.EndFile file);

	      if continu then iterFiles file (Some outStream) tl
	      else (Some outStream)
	    )
	)
    in
    let rec loop outStreamOpt =
      if mState != State.Stop && L.length mFiles > 0 then (
  	loop(iterFiles (L.hd mFiles) outStreamOpt mFiles)
      )
      else match outStreamOpt with
	| None -> ()
	| Some stream -> Portaudio.close_stream stream
    in
    mChangeFiles <- false;
    loop None;

    setState State.Stop;
    traceCyan"STOP";

end

let make() = new c()
