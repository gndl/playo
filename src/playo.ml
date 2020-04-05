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

let () =
  try
    let fl = A.fold_left ~f:(fun fl f -> if Sys.file_exists f then f::fl else fl)
	~init:[] (A.sub Sys.argv ~pos:1 ~len:(A.length Sys.argv - 1))
    in

    Player.initialize();

    let filesModel = new FilesModel.c in

    let ctrl = Controler.make filesModel ~filenameList:fl () in

    ignore(GtkMain.Main.init());

    let gui = new PlayoGui.mainWindow() in

    let _ = new MainWindow.c gui ctrl in

    let _ = new FilesView.c filesModel gui#audioFileTreeview ctrl gui#toplevel in

    GtkThread.main ();

    ctrl#release();

    Player.terminate();

  with e -> (
      traceMagenta(Printexc.to_string e);
      traceYellow(Printexc.get_backtrace());
      Printexc.record_backtrace true;
    );
