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

    let rec makeOutput file device =
      try
        let fmts = Avdevice.get_output_audio_devices() in

        let fmt = try
            List.find(fun d -> Av.Format.get_output_long_name d = device) fmts
          with Not_found -> List.hd fmts in

        let output = Av.open_output_format fmt in

        let dev = Av.Format.get_output_long_name fmt in

        if mOutputDevice <> dev || mNewOutputDevice <> dev then (
	  mOutputDevice <- dev;
	  mNewOutputDevice <- dev;
	  Ev.asyncNotify(Ev.OutputDeviceChanged self#getOutputDevice);
        );

        output
      with Avutil.Failure msg ->
	Ev.Error msg |> Ev.asyncNotify;

	(* If the new device raise an error, we fallback to the previous device *)
	if device <> mOutputDevice then
	  makeOutput file mOutputDevice
	else
	  raise (Avutil.Failure msg)
    in
    let defOutput file prevFile = function
      | None -> makeOutput file mNewOutputDevice
      | Some output ->
        if mNewOutputDevice <> mOutputDevice then (
          Av.close output;
	  makeOutput file mNewOutputDevice
        )
        else output
    in

    let open Bigarray in

    let playChunk file out =

      let channels = AudioFile.channels file in
      let talker = AudioFile.talker file in

      match Av.read talker.stream with
      | Av.Frame af -> (
          let buffer = InRsp.convert talker.input_resampler af in
          let readCount = Array1.dim buffer in
          let samplesPerChannel = readCount / channels in

	  for i = 0 to readCount - 1 do
	    buffer.{i} <- buffer.{i} *. mVolume;
          done;

          let continu = try
              buffer |> OutRsp.convert outputResampler |> Av.write_audio out;

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
      | Av.End_of_stream -> (
          AudioFile.resetPosition file;
	  false
        )
      | exception Avutil.Failure msg -> Ev.Error msg |> Ev.asyncNotify; false
    in
    let rec playFile file out =

      if mChangeFiles then (
        mChangeFiles <- false;
        (false, out)
      )
      else (
        let out =
          if mNewOutputDevice <> mOutputDevice then (
	    defOutput file file (Some out)
	  )
	  else out
        in
        match mState with
        | State.Play ->
	  if playChunk file out then
	    playFile file out
	  else
	    (true, out)
        | State.Pause -> Ev.asyncNotify(Ev.PauseFile file); Mutex.lock mPauseLock;
	  Mutex.unlock mPauseLock; Ev.asyncNotify(Ev.StartFile file);
	  playFile file out
        | State.Stop -> Ev.asyncNotify(Ev.State mState); (false, out)
      )
    in
    let rec iterFiles prevFile outOpt = function
      | [] -> Ev.asyncNotify(Ev.EndList prevFile); outOpt
      | file::tl -> (
  	  Ev.asyncNotify(Ev.StartFile file);
  	  match file.voice with
  	  | None -> Ev.asyncNotify(Ev.EndFile file); outOpt
  	  | Some talker -> (
	      let os = defOutput file prevFile outOpt in

              AudioFile.checkSeek file |> ignore;

	      let (continu, out) = playFile file os in

	      Ev.asyncNotify(Ev.EndFile file);

	      if continu then iterFiles file (Some out) tl
	      else (Some out)
	    )
        )
    in
    let rec loop outOpt =
      if mState != State.Stop && L.length mFiles > 0 then (
        loop(iterFiles (L.hd mFiles) outOpt mFiles)
      )
      else match outOpt with
        | None -> ()
        | Some output -> Av.close output;
    in
    mChangeFiles <- false;
    loop None;

    setState State.Stop;
    traceCyan"STOP";

end

let make() = new c()
