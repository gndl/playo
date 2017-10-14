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

module OutRsp = Swresample.Make (Swresample.FltBigArray) (Swresample.Frame)
module Ev = EventBus

let maxA = 1.

let initialize() = ()
let terminate() = ()


class c () = object (self)
  val mutable mFiles : AudioFile.t list = []
  val mutable mState = State.Stop
  val mutable mThread = Thread.self()
  val mPauseLock = Mutex.create()
  val mutable mChangeFiles = false
  val mutable mVolume = maxA /. 2.
  val mutable mOutputDevice = ""
  val mutable mNewOutputDevice = ""


  initializer
    mOutputDevice <- Avdevice.get_output_audio_devices()
                     |> List.hd
                     |> Av.Format.get_output_long_name;
    mNewOutputDevice <- mOutputDevice;


  method getState = mState

  method isPlaying = mState = State.Play

  method changeFiles files =
    mFiles <- files;
    mChangeFiles <- true;
    if mState <> State.Play && L.length files > 0 then self#play;


  method setVolume volumePercent = mVolume <- (volumePercent *. maxA) /. 100.

  method getOutputDevice = mNewOutputDevice


  method changeOutputDevice newOutputDeviceName = mNewOutputDevice <- newOutputDeviceName


  method getOutputDevices =
    Avdevice.get_output_audio_devices()
    |> List.map Av.Format.get_output_long_name

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

    let outputResampler = OutRsp.create outChannelLayout outRate
        outChannelLayout ~out_sample_format:outSampleFormat outRate in

    let rec makeOutputStream file device =
      try
        let fmts = Avdevice.get_output_audio_devices() in

        let fmt = try
            List.find(fun d -> Av.Format.get_output_long_name d = device) fmts
          with Not_found -> List.hd fmts in

        let o = Av.open_output_format fmt in
        let codec_id = Av.Format.get_audio_codec_id fmt in

        let stream = Av.new_audio_stream ~codec_id o in
        (* outputResampler <- OutRsp.to_codec outChannelLayout outSampleFormat outRate outChannelLayout outSa *)
        let dev = Av.Format.get_output_long_name fmt in

        if mOutputDevice <> dev || mNewOutputDevice <> dev then (
	  mOutputDevice <- dev;
	  mNewOutputDevice <- dev;
	  Ev.asyncNotify(Ev.OutputDeviceChanged self#getOutputDevice);
        );

        stream
      with Avutil.Failure msg ->
	Ev.Error msg |> Ev.asyncNotify;

	(* If the new device raise an error, we fallback to the previous device *)
	if device <> mOutputDevice then
	  makeOutputStream file mOutputDevice
	else
	  raise (Avutil.Failure msg)
    in
    let defOutputStream file prevFile = function
      | None -> makeOutputStream file mNewOutputDevice
      | Some stream ->
        if mNewOutputDevice <> mOutputDevice then (
          Av.get_output stream |> Av.close_output;
	  makeOutputStream file mNewOutputDevice
        )
        else stream
    in

    let open Bigarray in

    let playChunk file outStream =

      let channels = AudioFile.channels file in
      let talker = AudioFile.talker file in

      match Av.read talker.stream with
      | Av.Frame af -> (
          let buffer = InRsp.convert talker.input_resampler af in
          let readCount = Array1.dim buffer in
          let samplesPerChannel = readCount / channels in

          if mVolume < 0.8 then (
	    for i = 0 to readCount - 1 do
	      buffer.{i} <- buffer.{i} *. mVolume;
            done;
          );

          let continu = try
              buffer |> OutRsp.convert outputResampler |> Av.write outStream;
              AudioFile.addToPosition file samplesPerChannel;

	      Ev.asyncNotify(Ev.FileChanged file);
              true
            with Avutil.Failure msg -> (
                traceRed msg;
	          Ev.Error msg |> Ev.asyncNotify; false
	      )
          in
	  continu
        )
      | Av.End_of_file -> (
          AudioFile.resetPosition file;
	  false
        )
      | exception Avutil.Failure msg -> Ev.Error msg |> Ev.asyncNotify; false
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
        | Some stream -> Av.get_output stream |> Av.close_output;
    in
    mChangeFiles <- false;
    loop None;

    setState State.Stop;
    traceCyan"STOP";

end

let make() = new c()
