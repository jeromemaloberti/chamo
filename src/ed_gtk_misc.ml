(** *)


let treat_gtk_events () =
  while Glib.Main.pending () do
    ignore (Glib.Main.iteration false)
  done

let get_wm_window_position_offset () =
  let win = GWindow.window ~width: 0 ~height: 0 () in
  win#show ();
  let (x,y) = Gdk.Window.get_position win#misc#window in
  win#move ~x ~y;
  treat_gtk_events ();
  let (x2,y2) = Gdk.Window.get_position win#misc#window in
  win#destroy ();
  (x2 - x, y2 - y)

