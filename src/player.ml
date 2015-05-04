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

module Ev = AppEvent

class c () = object (self)
	val mutable mFiles : AudioFile.t list = []
	val mutable mState = State.Stop
	val mutable mThread = Thread.self()
	val mPauseLock = Mutex.create()
	val mutable mChangeFiles = false
	val mutable mVolume = maxA


	initializer
		Ev.addObserver (self :> Ev.observer);


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
			| State.Play -> ()(*trace "already playing"*)
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
		let outBuf = S.create AudioFile.outBufLen in

		(*try*)
		let dev = Ao.open_live ~driver:(Ao.find_driver "pulse") () in

		let playChunk file tkr =

			let readcount = Sndfile.read tkr.stream tkr.buf in

			if readcount > 0 then (
				let rec write ip op =
					let r = iof (tkr.buf.(ip) *. mVolume) in
					let l = iof (tkr.buf.(ip + 1) *. mVolume) in
					let rpfb = coi (r land 0xff) in
					let rpfr = coi ((r lsr 8) land 0xff) in
					let lpfb = coi (l land 0xff) in
					let lpfr = coi ((l lsr 8) land 0xff) in

					outBuf.[op] <- rpfb;
					outBuf.[op + 1] <- rpfr;
					outBuf.[op + 2] <- lpfb;
					outBuf.[op + 3] <- lpfr;

					if ip < readcount - tkr.nbChs then write (ip + tkr.nbChs) (op + 4)
				in
				write 0 0;

				Ao.play dev outBuf;
				
				if file.newFrame = file.curFrame then (
					let framesRead = Int64.of_int(readcount / tkr.nbChs) in
					file.newFrame <- Int64.add file.curFrame framesRead;
				)
				else (
(*					let offset = Int64.sub file.newFrame file.curFrame in
					file.newFrame <- Sndfile.seek tkr.stream offset Sndfile.SEEK_CUR;*)
					ignore(Sndfile.seek tkr.stream file.newFrame Sndfile.SEEK_SET);
				);
				
				AudioFile.setReadPercent file (Int64.to_int(Int64.div(Int64.mul file.newFrame
					(Int64.of_int 100)) (Sndfile.frames tkr.stream)));
					
				Ev.notify(Ev.FileChanged file);
				true
			)
			else (
				ignore(Sndfile.seek tkr.stream Int64.zero Sndfile.SEEK_SET);
				file.newFrame <- Int64.zero;
				file.curFrame <- Int64.zero;
				(*file.readPercent <- 0;
				Ev.notify(FileChanged file);*)
				false)
		in
		let playFile file =
			let rec eventLoop talker =
	
				if mChangeFiles then (
					mChangeFiles <- false;
					false
				)
				else (
					match mState with
					| State.Play -> if playChunk file talker then eventLoop talker else true
					| State.Pause -> Ev.notify(Ev.PauseFile file); Mutex.lock mPauseLock;
						Mutex.unlock mPauseLock; Ev.notify(Ev.StartFile file); eventLoop talker
					| State.Stop -> Ev.notify(Ev.State mState); false
				)
			in
			Ev.notify(Ev.StartFile file);
			match file.voice with
			| None -> Ev.notify(Ev.EndFile file); true
			| Some tkr -> (
				if file.newFrame <> file.curFrame then (
					ignore(Sndfile.seek tkr.stream file.newFrame Sndfile.SEEK_SET);
					file.curFrame <- file.newFrame;
				);
				let continu = eventLoop tkr in Ev.notify(Ev.EndFile file); continu)
		in
		let rec iter prevFile = function
			| [] -> Ev.notify(Ev.EndList prevFile)
			| f::tl -> if playFile f then iter f tl else ()
		in
		while mState != State.Stop && L.length mFiles > 0 do
			iter (L.hd mFiles) mFiles done;

		Ao.close dev;

		self#threadSetState State.Stop;
		traceCyan"STOP";

		(*with x -> Ev.notify(Error "faile to open device");*)

end

let make = new c()

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

