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

let _ =
	try
	let fl = A.fold_left(fun fl f -> if Sys.file_exists f then f::fl else fl)
			[] (A.sub Sys.argv 1 (A.length Sys.argv - 1))
	in
	
	Player.initialize();

	let ctrl = Controler.make ~filenameList:fl () in

	ignore(GtkMain.Main.init());

	let mainWindow = new MainWindow.c ctrl in
	let filesModel = new FilesModel.c in

	ctrl#init();
	filesModel#setNodes ctrl#nodes;

	let filesView = new FilesView.c filesModel 
		mainWindow#treeView ctrl mainWindow#toplevel in

	filesView#init();
  
	GtkThread.main ();

	ctrl#release();

	Player.terminate();

	with e -> (
		traceMagenta(Printexc.to_string e);
		traceYellow(Printexc.get_backtrace());
		Printexc.record_backtrace true;
	);
