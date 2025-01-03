/*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define OCAML_MORE_RAISE_SYS_ERROR(ERR)                             \
  do { caml_raise_sys_error (caml_copy_string("more stubs: " ERR)); } \
  while (0)

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_MORE_DARWIN
#endif

#if defined(__unix__) || defined(__unix)
  #include <unistd.h>
  #if defined(_POSIX_VERSION)
    #define OCAML_MORE_POSIX
  #endif
#endif

#if defined (__CYGWIN__)
  #define OCAML_MORE_CYGWIN
#endif

#if defined (_WIN32)
  #define OCAML_MORE_WINDOWS
  #define WIN32_LEAN_AND_MEAN
#endif
