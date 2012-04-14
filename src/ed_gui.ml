(*********************************************************************************)
(*                Chamo                                                          *)
(*                                                                               *)
(*    Copyright (C) 2003-2012 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Main gui classes *)

let window_pixmap =
  Filename.concat Cam_installation.pixmaps_dir "chamo.png"

let window_pixbuf =
  try Some (GdkPixbuf.from_file window_pixmap)
  with _ -> None

let init_view topwin (v : Ed_view.gui_view) =
  v#set_on_focus_in (fun _ -> topwin#set_active_view (Some v))

let insert_in_pos ele p =
  let rec iter n = function
      [] -> [ele]
    | h :: q ->
        if n = p
        then ele :: h :: q
        else h :: (iter (n+1) q)
  in
  iter 0


let label_of_contents = function
    `View o -> o#label
  | `Paned o -> o#label
  | `Notebook o -> o#label

let id_of_contents = function
    `View o -> Oo.id o
  | `Paned o -> Oo.id o
  | `Notebook o -> Oo.id o

let widget_of_contents = function
    `View gv -> gv#box
  | `Notebook gn -> gn#notebook#coerce
  | `Paned gp -> gp#paned#coerce

let contents_grab_focus = function
    `View gv -> gv#grab_focus
  | `Notebook gn -> gn#grab_focus
  | `Paned gp -> gp#grab_focus

let rec find_container c = function
    `View gv -> None
  | `Paned gp ->
      begin
        let pred c2 = id_of_contents c2 = id_of_contents c in
        let search_child = function
            None -> None
          | Some c2 ->
              if pred c2 then
                Some (`Paned gp)
              else
                find_container c c2
        in
        match search_child gp#child1 with
        | None -> search_child gp#child2
        | Some x -> Some x
      end
  | `Notebook gn ->
        let pred c2 = id_of_contents c2 = id_of_contents c in
        let rec iter = function
            [] -> None
          | (_,c2) :: q ->
              if pred c2 then
                Some (`Notebook gn)
              else
                match find_container c c2 with
                  None -> iter q
                | Some x -> Some x
        in
        iter gn#tabs

class gui_window ?x ?y ?width ?height () =
  let key_bindings_trees = ref [] in
  let minibuffer = new Ed_minibuffer.minibuffer () in
  object(self)
    inherit Ed_gui_base.main ~file: Ed_config.glade_file ()
    method x =
      fst (Gdk.Window.get_position toplevel#misc#window)
    method y =
      snd (Gdk.Window.get_position toplevel#misc#window)
    method width =
      fst (Gdk.Drawable.get_size toplevel#misc#window)
    method height =
      snd (Gdk.Drawable.get_size toplevel#misc#window)

    val mutable contents :
        [
          `Paned of gui_paned
        | `Notebook of gui_notebook
        | `View of Ed_view.gui_view
        ] option = None

    method window = toplevel
    method contents = contents
    method minibuffer = minibuffer

    val mutable active_view : Ed_view.gui_view option = None
    method set_active_view gvopt =
      active_view <- gvopt;
      key_bindings_trees := Ed_gui_rc.trees_for_window
          (match gvopt with None -> [] | Some v -> v#key_bindings);
      self#set_view_interface gvopt;
      minibuffer#set_active false
(*      prerr_endline "active view set!";*)

    method set_view_interface gvopt =
      menuEdit#remove_submenu ();
      let menu = GMenu.menu ~packing: menuEdit#set_submenu () in
      let l =
        match gvopt with
          None -> [false; false; false]
        | Some gv -> [gv#save <> None; gv#save_as <> None; gv#reload <> None]
      in
      List.iter2 (fun mi b -> mi#misc#set_sensitive b)
        [item_save;item_save_as;item_reload] l;
      let l =
        match gvopt with
          None -> [None;None;None]
        | Some gv -> [gv#paste;gv#copy;gv#cut]
      in
      List.iter2
        (fun label fopt ->
          let mi = GMenu.menu_item ~packing:menu#append ~label () in
          match fopt with
            None -> mi#misc#set_sensitive false
          | Some f ->
              ignore(mi#connect#activate f)
        )
        ["Paste";"Copy";"Cut"]
        l;
      let mb = viewmenubar in
      List.iter mb#remove mb#children;
      let f (label,entries) =
        let mi = GMenu.menu_item ~label ~packing: (mb#insert ~pos: 0) () in
        let mn = GMenu.menu ~packing: mi#set_submenu () in
        GToolbox.build_menu mn ~entries
      in
      match gvopt with
        None -> ()
      | Some gv -> List.iter f (List.rev gv#menus)

    method update_menus =
      self#set_view_interface self#active_view

    method active_view = active_view

    method contains_view (v : Ed_view.gui_view) =
      match contents with
        None -> false
      | Some (`View v2) -> Oo.id v = Oo.id v2
      | Some c -> find_container (`View v) c <> None

    method get_active_view_container =
      match active_view with
        None -> None
      | Some gv ->
          match contents with
            None -> None
          | Some (`View v) ->
              if Oo.id v = Oo.id gv then
                Some (`Window (self :> gui_window))
              else
                None
          | Some (`Paned gp) ->
              gp#find_view_container gv
          | Some (`Notebook gn) ->
              gn#find_view_container gv

    method destroy_active_view =
      match active_view with
        None -> ()
      | Some v -> v#destroy

    method on_about () =
      Ed_commands.eval_command "about"

    method on_new_window () =
      Ed_commands.eval_command "new_window"

    method on_open_file () =
      Ed_commands.eval_command "open_file"

    method on_destroy_active_view () =
      Ed_commands.eval_command "destroy_active_view"

    method reload_active_view =
      match active_view with
        None -> ()
      | Some gv ->
          match gv#reload with
            None -> ()
          | Some f -> f ()

    method ask_open_file =
      let dir =
        match active_view with
          None -> Sys.getcwd ()
        | Some v -> Filename.dirname v#filename
      in
      Ed_misc.select_file minibuffer
        ~title: "Open file" (Glib.Convert.filename_to_utf8 (dir^"/"))
        (fun s -> Ed_commands.eval_command (Printf.sprintf "open_file %s" (Filename.quote s)))

    method widget_opt_of_contents_opt = function
        None -> None
      | Some x -> Some (widget_of_contents x)

    method set_contents =
      begin
        match self#widget_opt_of_contents_opt contents with
          None -> ()
        | Some widget -> vbox#remove widget
      end;
      fun c ->
        contents <- c;
        match self#widget_opt_of_contents_opt c with
          None -> ()
        | Some widget ->
            begin
              match c with
              | None -> ()
              | Some (`View v) ->
                  v#set_on_destroy self#on_view_destroy;
                  v#set_on_label_change self#set_title
              | Some (`Notebook gn) ->
                  gn#set_on_destroy (fun c -> self#set_contents c);
                  gn#set_on_label_change self#set_title
              | Some (`Paned gp) ->
                  gp#set_on_destroy (fun c -> self#set_contents c);
                  gp#set_on_label_change self#set_title
            end;
            vbox#pack ~expand: true ~fill: true widget;
            vbox#reorder_child widget ~pos: 1;
            let label =
              match c with
                None -> ""
              | Some c -> contents_grab_focus c; label_of_contents c
            in
            self#set_title label

    method add_view v =
      match contents with
        None ->
          init_view (self :> Ed_view.topwin) v;
          self#set_contents (Some (`View v));
      | Some ((`View _) as current_c)
      | Some ((`Paned _) as current_c) ->
          (** TODO: whether a paned or notebook is created should
             be a choice in preferences *)
          let gn = new gui_notebook (self :> Ed_view.topwin) () in
          self#set_contents (Some (`Notebook gn));
          gn#add_tab None current_c;
          gn#add_view v
      | Some (`Notebook gn) ->
          gn#add_view v

    method private on_view_destroy () =
      match self#widget_opt_of_contents_opt contents with
        None -> ()
      | Some w ->
          contents <- None;
          vbox#remove w

    method add_view_in_active_view_container v =
      match self#get_active_view_container with
        None -> self#add_view v
      | Some (`Window _) -> self#add_view v
      | Some (`Paned gp) -> gp#add_view v
      | Some (`Notebook gn) -> gn#add_view v

    method open_file ?attributes f =
      try
        let factory = Ed_view.factory_of_filename f in
        match Ed_view.factory_open_file ~factory
          (self :> Ed_view.topwin) active_view ?attributes f
        with
          `Use_view v -> v#grab_focus
        | `New_view v ->
            init_view (self :> Ed_view.topwin) v;
            self#add_view_in_active_view_container v
      with
        Failure s ->
          self#error_message s

    method set_title s =
      let s = if s = "" then "" else ": "^s in
      toplevel#set_title (Printf.sprintf "%s%s" (Ed_misc.to_utf8 Ed_messages.software) s)

    method new_tab =
      match self#get_active_view_container with
      | None -> ()
      | Some (`Window _) ->
          (* it's me ! *)
          (
           match contents with
             Some (`View v) ->
               (
                match v#dup (self :> Ed_view.topwin) with
                  None -> ()
                | Some v ->
                    init_view (self :> Ed_view.topwin) v;
                    self#add_view v
               )
           | _ -> prerr_endline "Should not be here"
          )
      | Some (`Paned gp) ->
          (
           match active_view with
             None -> ()
           | Some v ->
               match v#dup (self :> Ed_view.topwin) with
                 None -> ()
               | Some v ->
                   init_view (self :> Ed_view.topwin) v;
                   gp#new_tab (`View v);
                   v#grab_focus;
          )
      | Some (`Notebook gn) ->
          (
           match active_view with
             None -> ()
           | Some v ->
               match v#dup (self :> Ed_view.topwin) with
                 None -> ()
               | Some v ->
                   init_view (self :> Ed_view.topwin) v;
                   gn#add_view v
          )

    method split_active_view (orientation : Gtk.Tags.orientation) =
      match self#get_active_view_container with
        None
      | Some (`Window _) ->
          begin
            match contents with
              Some (`View v1) ->
                (
                 match v1#dup (self :> Ed_view.topwin) with
                   None -> ()
                 | Some v2 ->
                     let gp = new gui_paned (self :> Ed_view.topwin) orientation () in
                     self#set_contents (Some (`Paned gp));
                     init_view (self :> Ed_view.topwin) v2;
                     gp#set_children_views v1 v2;
                     v2#grab_focus;
                )
            | _ -> ()
          end
      | Some (`Paned gp) -> gp#split_active_view orientation
      | Some (`Notebook gn) -> gn#split_active_view orientation

    method on_new_tab () = Ed_commands.eval_command "new_tab"

    method on_split_active_view o () =
      Ed_commands.eval_command
        (Printf.sprintf "split_%s"
           (match o with
             `HORIZONTAL -> "horizontally"
           | `VERTICAL -> "vertically"))

    method on_store_layout () = Ed_commands.eval_command "store_layout"

    method cycle_tab forward =
      match active_view with
        None -> ()
      | Some v ->
          match contents with
            None -> ()
          | Some mycontents ->
              let rec iter c =
                match find_container c mycontents with
                  None -> None
                | Some (`Notebook gn) ->
                    Some gn
                | Some x -> iter x
              in
              match iter (`View v) with
                None -> ()
              | Some gn -> gn#cycle_tab forward

    method cycle_view =
      match active_view with
        None -> ()
      | Some v ->
          match contents with
            None -> ()
          | Some mycontents ->
              let rec iter c =
                match find_container c mycontents with
                  None ->
                    begin
                      match c with
                        `Paned gp ->
                          begin
                            match gp#child1, gp#child2 with
                              Some c, _
                            | None, Some c -> contents_grab_focus c
                            | None, None -> ()
                          end
                      | `View v ->
                          (* shold not happen *)
                          v#grab_focus
                      | `Notebook nb ->
                          (* give focus to first view in the current tab *)
                          nb#grab_focus
                    end
                | Some ((`Paned gp) as x) ->
                    begin
                      match gp#child1, gp#child2 with
                      | Some c1, Some c2 when id_of_contents c1 = id_of_contents c ->
                          contents_grab_focus c2
                      | _ -> iter x
                    end
                | Some x -> iter x
              in
              iter (`View v)

    method on_close () = Ed_commands.eval_command "close_active_window"
    method close = self#window#destroy ()

    method set_state_message = wl_keystate#set_text

    method set_action_message msg =
      if minibuffer#active then
        ()
      else
        minibuffer#set_text msg

    method error_message s =
      Ed_misc.error_message (Ed_misc.to_utf8 s)

    method save_active_view =
      match active_view with
        None -> ()
      | Some v ->
          match v#save with
            None -> ()
          | Some f -> f ()

    method save_active_view_as =
      match active_view with
        None -> ()
      | Some v ->
          match v#save_as with
            None -> ()
          | Some f -> f ()

    method display_keyhit_state ~after_handler st =
      let s = Ed_keymaps.string_of_state st in
      self#set_state_message s ;
      if not after_handler then
        self#set_action_message ""

    method print_key_bindings =
      let l = Ed_gui_rc.window_key_bindings#get @
        (match active_view with
          None -> []
        | Some v -> v#key_bindings)
      in
      List.iter
        (fun (st,com) ->
           Ed_hooks.display_message (Printf.sprintf "%s -> %s"
            (Ed_keymaps.string_of_state st) com))
        l

    method on_minibuffer_active_change active =
      if active then
        begin
          Okey.reset_state toplevel;
          self#display_keyhit_state ~after_handler: true [];
          key_bindings_trees := Ed_gui_rc.trees_for_window minibuffer#key_bindings
        end
      else
        match active_view with
          None -> () | Some v -> v#grab_focus

    method paste =
      match active_view with
        None -> ()
      | Some v ->
          match v#paste with
            None -> ()
          | Some f -> f ()
    method copy =
      match active_view with
        None -> ()
      | Some v ->
          match v#copy with
            None -> ()
          | Some f -> f ()
    method cut =
      match active_view with
        None -> ()
      | Some v ->
          match v#cut with
            None -> ()
          | Some f -> f ()

    initializer
      toplevel#set_icon window_pixbuf;
      hbox_state#pack ~expand: true ~fill: true minibuffer#box;
      minibuffer#set_on_active_change self#on_minibuffer_active_change;
      begin
        match x, y with
          Some x, Some y -> toplevel#move ~x ~y
        | _ -> ()
      end;
      begin
        match width, height with
          Some width, Some height -> toplevel#resize ~width ~height
        | _ -> ()
      end;
      let handlers =
        [
          ("on_close_activate",               `Simple self#on_close);
          ("on_log_window_activate",          `Simple (fun () -> Ed_commands.eval_command "log_window"));
          ("on_save_activate",                `Simple (fun () -> Ed_commands.eval_command "save_active_view"));
          ("on_save_as_activate",             `Simple (fun () -> Ed_commands.eval_command "save_active_view_as"));
          ("on_reload_activate",              `Simple (fun () -> Ed_commands.eval_command "reload_active_view"));
          ("on_about_activate",               `Simple self#on_about);
          ("on_new_window_activate",          `Simple self#on_new_window);
          ("on_open_file_activate",           `Simple self#on_open_file);
          ("on_new_tab_activate",             `Simple self#on_new_tab);
          ("on_split_horizontally_activate",  `Simple (self#on_split_active_view `HORIZONTAL));
          ("on_split_vertically_activate",    `Simple (self#on_split_active_view `VERTICAL));
          ("on_destroy_active_view_activate", `Simple self#on_destroy_active_view);
          ("on_store_layout_activate",        `Simple self#on_store_layout);
          ("on_cycle_tab_activate",           `Simple (fun () -> Ed_commands.eval_command "cycle_tab"));
          ("on_cycle_view_activate",          `Simple (fun () -> Ed_commands.eval_command "cycle_view"));
          ("on_preferences_activate",         `Simple Ed_prefs.edit_preferences) ;
        ]
      in
      (* Finalize GUI *)
      Glade.bind_handlers ~extra:handlers ~warn:true self#xml;

      key_bindings_trees := Ed_gui_rc.trees_for_window [];
      Okey.set_handler_trees
        ~stop: Ed_gui_rc.abort_binding#get
        (fun () -> !key_bindings_trees)
        ~f_display_state: self#display_keyhit_state
        toplevel;

  end

and gui_paned (topwin : Ed_view.topwin) orientation () =
  let paned = GPack.paned orientation () in
  object(self)
    inherit Ed_view.dyn_label

    method paned = paned
    method orientation = orientation
    method position = paned#position
    method set_position = paned#set_position

    val mutable child1 :
        [
          `Paned of gui_paned
        | `Notebook of gui_notebook
        | `View of Ed_view.gui_view
        ] option = None
    val mutable child2 :
        [
          `Paned of gui_paned
        | `Notebook of gui_notebook
        | `View of Ed_view.gui_view
        ] option = None

    method child1 = child1
    method child2 = child2

    val mutable on_destroy =
      (fun (c :  [ `Paned of gui_paned
                | `Notebook of gui_notebook
                | `View of Ed_view.gui_view
                ] option) -> ())
    method on_child_view_destroy n =
      List.iter paned#remove paned#children;
      on_destroy (if n = 1 then child2 else child1);
      child1 <- None;
      child2 <- None;
      paned#destroy()

    method set_on_destroy f = on_destroy <- f

    method on_child_destroy n c =
      match (if n = 1 then child1 else child2) with
        None -> () (* strange *)
      | Some c_old ->
          match c with
            None -> (* strange, nothing to replace the child,
                       we do as if it was a view so that the other child
                       replaces the paned in the parent *)
              self#on_child_view_destroy n
          | Some c ->
              let w = widget_of_contents c_old in
              paned#remove w;
              (if n = 1 then paned#add1 else paned#add2) (widget_of_contents c);
              if n = 1 then child1 <- Some c else child2 <- Some c;
              self#on_child_label_change;
              contents_grab_focus c

    method on_child_label_change =
      let s =
        match child1, child2 with
          None, Some c
        | Some c, None -> label_of_contents c
        | None, None -> " "
        | Some c1, Some c2 -> Printf.sprintf "%s | %s"
              (label_of_contents c1) (label_of_contents c2)
      in
      self#set_label s

    method set_one_child n c =
      begin
        match (if n = 1 then child1 else child2) with
          None -> ()
        | Some c ->
            paned#remove (widget_of_contents c)
      end;
      if n = 1 then child1 <- Some c else child2 <- Some c;
      begin
        match c with
          `View v ->
            v#set_on_destroy (fun () -> self#on_child_view_destroy n);
            v#set_on_label_change (fun _ -> self#on_child_label_change);
        | `Paned gp ->
            gp#set_on_destroy (self#on_child_destroy n);
            gp#set_on_label_change (fun _ -> self#on_child_label_change);
        | `Notebook gn ->
            gn#set_on_destroy (self#on_child_destroy n);
            gn#set_on_label_change (fun _ -> self#on_child_label_change);
      end;
      (if n = 1 then paned#add1 else paned#add2) (widget_of_contents c);
      self#on_child_label_change;
      contents_grab_focus c

    method set_children_views v1 v2 =
      self#set_one_child 1 (`View v1);
      self#set_one_child 2 (`View v2);
      let (w,h) =
        Gdk.Drawable.get_size
          (Gdk.Drawable.cast paned#misc#window)
      in
      let p = match orientation with
        `VERTICAL -> h
      | `HORIZONTAL -> w
      in
      paned#set_position (p / 2)

    method find_view_container gv =
      let find_in_child = function
          None -> None
        | Some (`Notebook gn) ->
            gn#find_view_container gv
        | Some (`Paned gp) ->
            gp#find_view_container gv
        | Some (`View v) ->
            if Oo.id v = Oo.id gv then
              Some (`Paned (self :> gui_paned))
            else
              None
      in
      match find_in_child child1 with
        None -> find_in_child child2
      | Some x -> Some x

    method new_tab c =
      (* we insert the tab in a view; if we don't have a
         the active view, then we should not be here since
         the new_tab is inserted in the active view container *)
      match topwin#active_view with
        None -> ()
      | Some v ->
          let res =
            match child1 with
              Some (`View v1) when Oo.id v1 = Oo.id v -> Some (1, v1)
            | _ ->
                match child2 with
                  Some (`View v2) when Oo.id v2 = Oo.id v -> Some (2, v2)
                | _ -> None
          in
          match res with
            None -> prerr_endline "Can't insert tab here, we should not be here in this paned"
          | Some (n, cur_view) ->
              paned#remove cur_view#box;
              let gn = new gui_notebook topwin () in
              (if n = 1 then paned#add1 else paned#add2) gn#notebook#coerce;
              gn#set_on_destroy (self#on_child_destroy n);
              gn#set_on_label_change (fun _ -> self#on_child_label_change);
              if n = 1 then
                child1  <- Some (`Notebook gn)
              else
                child2  <- Some (`Notebook gn);
              gn#add_tab None (`View cur_view);
              gn#add_tab None c;

    method add_view (v : Ed_view.gui_view) =
       match topwin#active_view with
        None -> ()
      | Some av ->
          let res =
            match child1 with
              Some (`View v1) when Oo.id v1 = Oo.id av -> Some (1, v1)
            | _ ->
                match child2 with
                  Some (`View v2) when Oo.id v2 = Oo.id av -> Some (2, v2)
                | _ -> None
          in
          match res with
            None -> prerr_endline "Can't insert view here, we should not be here in this paned"
          | Some (n, cur_view) ->
              paned#remove cur_view#box;
              (* TODO: use a user preference to use a tab of a paned *)
              let gp = new gui_paned topwin orientation () in
              (if n = 1 then paned#add1 else paned#add2) gp#paned#coerce;
              gp#set_on_destroy (self#on_child_destroy n);
              gp#set_on_label_change (fun _ -> self#on_child_label_change);
              if n = 1 then
                child1  <- Some (`Paned gp)
              else
                child2  <- Some (`Paned gp);
              gp#set_children_views cur_view v;
              v#grab_focus

    method split_active_view orientation =
      match topwin#active_view with
        None -> ()
      | Some v ->
          let res =
            match child1 with
              Some (`View v1) when Oo.id v1 = Oo.id v -> Some (1, v1)
            | _ ->
                match child2 with
                  Some (`View v2) when Oo.id v2 = Oo.id v -> Some (2, v2)
                | _ -> None
          in
          match res with
            None -> prerr_endline "can't split this, not a view"
          | Some (n, cur_view) ->
              match cur_view#dup topwin with
                None -> ()
              | Some view_copy ->
                  let gp = new gui_paned topwin orientation () in
                  paned#remove cur_view#box;
                  if n = 1 then
                    child1  <- Some (`Paned gp)
                  else
                    child2  <- Some (`Paned gp);
                  (if n = 1 then paned#add1 else paned#add2) gp#paned#coerce;
                  gp#set_on_label_change (fun _ -> self#on_child_label_change);
                  gp#set_on_destroy (self#on_child_destroy n);
                  init_view topwin view_copy;
                  gp#set_children_views cur_view view_copy;
                  view_copy#grab_focus

    method grab_focus =
      match child1 with
        Some c -> contents_grab_focus c
      | None ->
          match child2 with
            Some c -> contents_grab_focus c
          | None -> ()

  end

and gui_notebook (topwin : Ed_view.topwin) () =
  let nb = GPack.notebook
      ~border_width: 0
      ~show_border: false
      ~scrollable: true () in
  object(self)
    inherit Ed_view.dyn_label
    method notebook = nb

    val mutable tabs :
        (GMisc.label *
           [
             `Paned of gui_paned
           | `Notebook of gui_notebook
           | `View of Ed_view.gui_view
           ]
        ) list = []

    method tabs = tabs

    val mutable on_destroy =
      (fun (c :  [ `Paned of gui_paned
                | `Notebook of gui_notebook
                | `View of Ed_view.gui_view
                ] option) -> ())
    method destroy =
      match tabs with
        (_,c) :: _ ->
          for i = 0 to List.length tabs - 1 do
            nb#remove_page i
          done;
          on_destroy (Some c);
          nb#destroy()
      | [] -> on_destroy None; nb#destroy ()

    method set_on_destroy f = on_destroy <- f

    method on_tab_destroy c new_c =
      match self#tab_of_contents c with
        None -> ()
      | Some n ->
          tabs <- List.filter
              (fun (_,c2) -> id_of_contents c2 <> id_of_contents c) tabs;
          nb#remove_page n;
          match new_c, tabs with
            None, []
          | None, [_] -> self#destroy
          | None, _ -> ()
          | Some new_c, [] ->
              on_destroy (Some new_c);
              nb#destroy ()
          | Some new_c, _ ->
              self#add_tab (Some n) new_c

    method on_view_destroy v () = self#on_tab_destroy (`View v) None

    method find_view_container :
       Ed_view.gui_view -> [ `Notebook of gui_notebook
                        | `Paned of gui_paned
                        | `Window of gui_window ] option =
         fun gv ->
           let find_in_child = function
             | `Notebook gn ->
                 gn#find_view_container gv
             | `Paned gp ->
                 gp#find_view_container gv
             | `View v ->
                 if Oo.id v = Oo.id gv then
                   Some (`Notebook (self :> gui_notebook))
                 else
                   None
           in
           let rec iter = function
               [] -> None
             | (_,h) :: q ->
                 match find_in_child h with
                   None -> iter q
                 | Some x -> Some x
           in
           iter self#tabs

    method tab_of_contents c =
      let oid = id_of_contents c in
      let pred c2 = id_of_contents c2 = oid in
      let rec iter n = function
          [] -> None
        | (_,h) :: q ->
            if pred h then Some n else iter (n+1) q
      in
      iter 0 tabs

    method add_view v =
      self#add_tab None (`View v);
      v#grab_focus

    method set_tab_label c s =
      match self#tab_of_contents c with
        None -> ()
      | Some n ->
          let (w,_) = List.nth tabs n in
          w#set_text (Ed_misc.to_utf8 (label_of_contents c));
          self#set_label s

    method add_tab pos c =
      let label = label_of_contents c in
      let wlabel = GMisc.label ~text: label() in
      tabs <-
        (match pos with
          None -> tabs @ [wlabel,c]
        | Some pos -> insert_in_pos (wlabel,c) pos tabs
        );
      let w = match c with
        `View gv ->
          gv#set_on_label_change (self#set_tab_label c);
          gv#set_on_destroy (self#on_view_destroy gv);
          gv#box
      | `Notebook gn ->
          gn#set_on_label_change (self#set_tab_label c);
          gn#set_on_destroy (self#on_tab_destroy c);
          gn#notebook#coerce
      | `Paned gp ->
          gp#set_on_label_change (self#set_tab_label c);
          gp#set_on_destroy (self#on_tab_destroy c);
          gp#paned#coerce
      in
      let n =
        match pos with
          None ->
            ignore(nb#append_page ~tab_label: wlabel#coerce w);
            List.length tabs - 1
        | Some pos ->
            ignore(nb#insert_page ~pos ~tab_label: wlabel#coerce w);
            pos
      in
      self#goto_page n;
      contents_grab_focus c

    method split_active_view orientation =
      match topwin#active_view with
        None -> ()
      | Some v1 ->
          match self#tab_of_contents (`View v1) with
            None -> ()
          | Some n ->
              let (wl,_) = List.nth tabs n in
              match v1#dup topwin with
                None -> ()
              | Some v2 ->
                  let gp = new gui_paned topwin orientation () in
                  gp#set_on_label_change (self#set_tab_label (`Paned gp));
                  gp#set_on_destroy (self#on_tab_destroy (`Paned gp));
                  nb#remove_page n;
                  let rec iter m = function
                      [] -> []
                    | (wl,`View v') :: q when n = m ->
                        (wl, `Paned gp) :: q
                    | h :: q ->
                        h :: (iter (m+1) q)
                  in
                  tabs <- iter 0 tabs;
                  ignore(nb#insert_page ~tab_label: wl#coerce ~pos: n gp#paned#coerce);
                  init_view topwin v2;
                  gp#set_children_views v1 v2;
                  self#goto_page n;
                  v2#grab_focus;

    method grab_focus =
      try
        let (_,c) = List.nth tabs nb#current_page in
        contents_grab_focus c
      with _ -> ()

    method cycle_tab forward =
      let new_page =
        ((if forward then (+) else (-)) nb#current_page 1) mod (List.length tabs)
      in
      self#goto_page new_page

    method goto_page n = nb#goto_page n

    method on_switch_page n =
      self#set_label (label_of_contents (snd (List.nth tabs n)))

    initializer
      ignore (nb#connect#after#switch_page self#on_switch_page);
  end

type gui_windows = gui_window list

let gui_windows : gui_windows ref = ref []

let active_window = ref (None : gui_window option)

let on_last_window_close = ref (fun () -> ())

let on_window_destroy w () =
  gui_windows :=
    List.filter (fun w2 -> Oo.id w <> Oo.id w2) !gui_windows;
  match !gui_windows with
    [] -> !on_last_window_close ()
  | _ -> ()

let create_window ?x ?y ?width ?height () =
  let o = new gui_window ?x ?y ?width ?height () in
  gui_windows := o :: !gui_windows;
  let w = o#window in
  ignore(w#connect#destroy (on_window_destroy o));
  ignore(w#event#connect#focus_in
           (fun _ -> active_window := Some o; false));
  w#show ();
  o

let _ = Ed_commands.register (Ed_commands.unit_com "new_window"
                             (fun () -> ignore(create_window ())))

let in_new_window args =
  let com = Ed_commands.argv_to_string args in
  let w = create_window () in
  active_window := Some w;
  Ed_commands.eval_command com

let _ =
  let com = Ed_commands.create_com "in_new_window"
      ~more: "command and arguments to launch with the new window active"
      [| |]
      in_new_window
  in
  Ed_commands.register com

let about_dialog = ref (fun () -> raise Not_found)
let show_about_dialog () =
  try !about_dialog ()
  with Not_found ->
      let dialog = GWindow.about_dialog
        ~authors:
          [(Printf.sprintf "%s <%s>"
             Ed_messages.software_author
               Ed_messages.software_author_mail)]
          ~name: Ed_messages.software
          ~version: Cam_installation.software_version
          ~website: "http://www.gna.org/projects/cameleon"
          ~website_label: "The Cameleon website"
          ~position: `CENTER
          ~copyright: Ed_messages.software_copyright
          ~logo: (GdkPixbuf.from_file window_pixmap)
          ~modal: true
          ()
      in
      about_dialog := dialog#present ;
      ignore(dialog#connect#response (fun _ -> dialog#misc#hide()));
      dialog#show ()
;;

let _ = Ed_commands.register (Ed_commands.unit_com "about" show_about_dialog)

let on_active_window f () =
  match !active_window with
    None -> Ed_hooks.warning_message "no active window.";
  | Some o -> f o

let on_active_window_args (f : gui_window -> string array -> unit) args =
  (match !active_window with
    None ->
       fun _ ->
         Ed_hooks.warning_message "no active window.";
  | Some o -> f o
  ) args

let _ =
  let f args =
    let len = Array.length args in
    if len <= 0 then
      let g w = w#ask_open_file in
      on_active_window g ()
    else
      let filename = args.(0) in
      let loc = if len = 1 then None else Some args.(1) in
      let attributes =
        match loc with
          None -> None
        | Some loc -> Some ["location", loc]
      in
      let g w = w#open_file ?attributes filename in
      on_active_window g ()
  in
  let com =
    { Ed_commands.com_name = "open_file" ;
      Ed_commands.com_args = [| "file" |] ;
      Ed_commands.com_more_args = Some "optional location" ;
      Ed_commands.com_f = f;
    }
  in
  Ed_commands.register com
;;
let open_file_with_encoding args =
  let len = Array.length args in
  if len < 1 then
     let g w =
       let dir =
         match w#active_view with
          None -> Sys.getcwd ()
        | Some v -> Filename.dirname v#filename
      in
      Ed_misc.select_file w#minibuffer
        ~title: "Open file with encoding" (Glib.Convert.filename_to_utf8 (dir^"/"))
        (fun s -> Ed_commands.launch_command "open_file_with_encoding" [|s|])
    in
    on_active_window g ()
  else if len < 2 then
      let g w =
        let f s = Ed_commands.launch_command "open_file_with_encoding" [| args.(0); s |] in
        Ed_misc.select_string w#minibuffer
          ~title: (Printf.sprintf "Open %s with encoding" (Glib.Convert.filename_to_utf8 args.(0)))
          ~choices: Ed_charsets.charsets
          "" f
      in
      on_active_window g ()
  else
    let g (w : gui_window) = w#open_file ~attributes: ["encoding", args.(1)] args.(0) in
    on_active_window g ()
;;

Ed_commands.register
  { Ed_commands.com_name = "open_file_with_encoding" ;
    Ed_commands.com_args = [| "file"; "encoding" |] ;
    Ed_commands.com_more_args = None ;
    Ed_commands.com_f = open_file_with_encoding;
  }
;;


let prompt_command_history = Ed_minibuffer.history ()
let prompt_command (w : gui_window) =
  let mb = w#minibuffer in
  let on_return com =
    match Ed_misc.no_blanks com with
      "" -> ()
    | _ -> Ed_commands.eval_command com
  in
  Ed_misc.select_string
    ~history: prompt_command_history
    mb ~title: "Command"
    ~choices: (Ed_commands.available_command_names ())
    ""
    on_return

let unit_coms_on_active_window =
  [
    "close_active_window",  (fun w -> w#close) ;
    "new_tab", (fun w -> w#new_tab) ;
    "split_vertically", (fun w -> w#split_active_view `HORIZONTAL) ;
    "split_horizontally", (fun w -> w#split_active_view `VERTICAL) ;
    "destroy_active_view", (fun w -> w#destroy_active_view) ;
    "cycle_tab", (fun w -> w#cycle_tab true) ;
    "rev_cycle_tab", (fun w -> w#cycle_tab false) ;
    "cycle_view", (fun w -> w#cycle_view) ;
    "save_active_view", (fun w -> w#save_active_view) ;
    "save_active_view_as", (fun w -> w#save_active_view_as) ;
    "reload_active_view", (fun w -> w#reload_active_view) ;
    "print_key_bindings", (fun w -> w#print_key_bindings) ;
    "paste", (fun w -> w#paste) ;
    "copy", (fun w -> w#copy) ;
    "cut", (fun w -> w#cut) ;
    "prompt_command", (fun w -> prompt_command w) ;

    Ed_minibuffer_rc.base_name ^ "_eval", (fun w -> w#minibuffer#eval) ;
    Ed_minibuffer_rc.base_name ^ "_complete", (fun w -> w#minibuffer#complete) ;
    Ed_minibuffer_rc.base_name ^ "_history_previous", (fun w -> w#minibuffer#history_previous) ;
    Ed_minibuffer_rc.base_name ^ "_history_next", (fun w -> w#minibuffer#history_next) ;
    Ed_minibuffer_rc.base_name ^ "_exit", (fun w -> w#minibuffer#exit ()) ;
  ]
let _ = List.iter
    (fun (name, f) ->
      Ed_commands.register (Ed_commands.unit_com name (on_active_window f)))
    unit_coms_on_active_window

let args_coms_on_active_window =
  [
    "set_active_state_message", [| "message" |],
    (fun w args -> w#set_state_message (if Array.length args > 0 then args.(0) else "")) ;

    "set_active_action_message", [| "message" |],
    (fun w args -> w#set_action_message (if Array.length args > 0 then args.(0) else ""));

    Ed_minibuffer_rc.base_name ^ "_eval_custom_key_binding", [| "binding" |],
    (fun w args -> if Array.length args > 0 then w#minibuffer#eval_custom_key_binding args.(0)) ;

    Ed_minibuffer_rc.base_name ^ "_insert", [| "string" |],
    (fun w args -> if Array.length args > 0 then w#minibuffer#insert args.(0)) ;
  ]

let _ = List.iter
    (fun (name,args, f) ->
      Ed_commands.register (Ed_commands.create_com name args (on_active_window_args f)))
    args_coms_on_active_window
