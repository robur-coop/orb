#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#include <stdlib.h>

CAMLprim value orb_unsetenv (value name) {
  CAMLparam1(name);
  const char *c_name;
  if (! caml_string_is_c_safe(name)) caml_raise_not_found();
  c_name = String_val(name);
  if (unsetenv(c_name) != 0) uerror("unsetenv", Nothing);
  CAMLreturn(Val_unit);
}
