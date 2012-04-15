(** Gtk utilities. *)

(** Return the offset of position due to the window manager's decoration. *)
val get_wm_window_position_offset : unit -> int * int

(** Handle all pending GTK events. *)
val treat_gtk_events : unit -> unit

