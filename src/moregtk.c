#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/callback.h>

#include <wrappers.h>
#include <gtk-2.0/gtk/gtkeventbox.h>
#include <gtk-2.0/gtk/gtktextview.h>

#define GtkEventBox_val(val) Pointer_val(val)
#define GtkTextView_val(val) Pointer_val(val)

ML_2(gtk_event_box_set_above_child,GtkEventBox_val,Bool_val,Unit)
