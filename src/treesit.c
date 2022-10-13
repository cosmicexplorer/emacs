/* Tree-sitter integration for GNU Emacs.

Copyright (C) 2021-2022 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "treesit.h"

/* Commentary

   The Emacs wrapper of tree-sitter does not expose everything the C
   API provides, most notably:

   - It doesn't expose a syntax tree, we put the syntax tree in the
     parser object, and updating the tree is handled in the C level.

   - We don't expose tree cursor either.  I think Lisp is slow enough
     to nullify any performance advantage of using a cursor, though I
     don't have evidence.  Also I want to minimize the number of new
     types we introduce, currently we only add parser and node type.

   - Because updating the change is handled in the C level as each
     change is made in the buffer, there is no way for Lisp to update
     a node.  But since we can just retrieve a new node, it shouldn't
     be a limitation.

   - I didn't expose setting timeout and cancellation flag for a
     parser, mainly because I don't think they are really necessary
     in Emacs' use cases.

   - Many tree-sitter functions asks for a TSPoint, basically a (row,
     column) location.  Emacs uses a gap buffer and keeps no
     information about row and column position.  According to the
     author of tree-sitter, tree-sitter only asks for (row, column)
     position to carry it around and return back to the user later;
     and the real position used is the byte position.  He also said
     that he _think_ that it will work to use byte position only.
     That's why whenever a TSPoint is asked, we pass a dummy one to
     it.  Judging by the nature of parsing algorithms, I think it is
     safe to use only byte position, and I don't think this will
     change in the future.

     REF: https://github.com/tree-sitter/tree-sitter/issues/445

   treesit.h has some commentary on the two main data structure
   for the parser and node.  ts_ensure_position_synced has some
   commentary on how do we make tree-sitter play well with narrowing
   (tree-sitter parser only sees the visible region, so we need to
   translate positions back and forth).  Most action happens in
   ts_ensure_parsed, ts_read_buffer and ts_record_change.

   A complete correspondence list between tree-sitter functions and
   exposed Lisp functions can be found in the manual (elisp)API
   Correspondence.

   Placement of CHECK_xxx functions: call CHECK_xxx before using any
   unchecked Lisp values; these include argument of Lisp functions,
   return value of Fsymbol_value, car of a cons.

   Initializing tree-sitter: there are two entry points to tree-sitter
   functions: 'treesit-parser-create' and
   'treesit-language-available-p'.  Therefore we only need to call
   initialization function in those two functions.

   Tree-sitter offset (0-based) and buffer position (1-based):
     tree-sitter offset + buffer position = buffer position
     buffer position - buffer position = tree-sitter offset

   Tree-sitter-related code in other files:
   - src/alloc.c for gc for parser and node
   - src/casefiddle.c & src/insdel.c for notifying tree-sitter
     parser of buffer changes.
   - lisp/emacs-lisp/cl-preloaded.el & data.c & lisp.h for parser and
     node type.

   Regarding signals: only raise signals in lisp functions.

   We don't parse at every keystroke.  Instead we only record the
   changes at each keystroke, and only parse when requested.  It is
   possible that lazy parsing is worse: instead of dispersed little
   pauses, now you have less frequent but larger pauses.  I doubt
   there will be any perceived difference, as the lazy parsing is
   going to be pretty frequent anyway.  Also this (lazy parsing) is
   what the mailing list guys wanted.

   Because it is pretty slow (comparing to other tree-sitter
   operations) for tree-sitter to parse the query and produce a query
   object, it is very wasteful to reparse the query every time
   treesit-query-capture is called, and it completely kills the
   performance of querying in a loop for a moderate amount of times
   (hundreds of queries takes seconds rather than milliseconds to
   complete).  Therefore we want some caching. We can either use a
   search.c style transparent caching, or simply expose a new type,
   compiled-ts-query and let the user to manually compile AOT.  I
   believe AOT compiling gives users more control, makes the
   performance stable and easy to understand (compiled -> fast,
   uncompiled -> slow), and avoids some edge cases transparent cache
   could have (see below).  So I implemented the AOT compilation.

   Problems a transparent cache could have: Suppose we store cache
   entries in a fixed-length linked-list, and compare with EQ.  1)
   One-off query could kick out useful cache.  2) if the user messed
   up and the query doesn't EQ to the cache anymore, the performance
   mysteriously drops.  3) what if a user uses so many stuff that the
   default cache size (20) is not enough and we end up thrashing?
   These are all imagined scenarios but they are not impossible :-)
 */

/*** Initialization */

bool ts_initialized = false;

static void *
ts_calloc_wrapper (size_t n, size_t size)
{
  return xzalloc (n * size);
}

static void
ts_initialize (void)
{
  if (!ts_initialized)
    {
      ts_set_allocator (xmalloc, ts_calloc_wrapper, xrealloc, xfree);
      ts_initialized = true;
    }
}

/*** Loading language library */

/* Translates a symbol treesit-<lang> to a C name
   treesit_<lang>.  */
static void
ts_symbol_to_c_name (char *symbol_name)
{
  for (int idx=0; idx < strlen (symbol_name); idx++)
    {
      if (symbol_name[idx] == '-')
	symbol_name[idx] = '_';
    }
}

static bool
ts_find_override_name
(Lisp_Object language_symbol, Lisp_Object *name, Lisp_Object *c_symbol)
{
  for (Lisp_Object list = Vtreesit_load_name_override_list;
       !NILP (list); list = XCDR (list))
    {
      Lisp_Object lang = XCAR (XCAR (list));
      CHECK_SYMBOL (lang);
      if (EQ (lang, language_symbol))
	{
	  *name = Fnth (make_fixnum (1), XCAR (list));
	  CHECK_STRING (*name);
	  *c_symbol = Fnth (make_fixnum (2), XCAR (list));
	  CHECK_STRING (*c_symbol);
	  return true;
	}
    }
  return false;
}

/* For example, if Vdynamic_library_suffixes is (".so", ".dylib"),
   thsi function pushes "lib_base_name.so" and "lib_base_name.dylib"
   into *path_candidates. Obiviously path_candidates should be a Lisp
   list of Lisp strings.  */
static void
ts_load_language_push_for_each_suffix
(Lisp_Object lib_base_name, Lisp_Object *path_candidates)
{
  for (Lisp_Object suffixes = Vdynamic_library_suffixes;
       !NILP (suffixes); suffixes = XCDR (suffixes)) {
    *path_candidates = Fcons (concat2 (lib_base_name, XCAR (suffixes)),
			      *path_candidates);
  }
}

/* Load the dynamic library of LANGUAGE_SYMBOL and return the pointer
   to the language definition.

   If error occurs, return NULL and fill SIGNAL_SYMBOL and SIGNAL_DATA
   with values suitable for xsignal. */
static TSLanguage *
ts_load_language (Lisp_Object language_symbol,
		  Lisp_Object *signal_symbol, Lisp_Object *signal_data)
{
  Lisp_Object symbol_name = Fsymbol_name (language_symbol);

  /* Figure out the library name and C name.  */
  Lisp_Object lib_base_name =
    (concat2 (build_pure_c_string ("libtree-sitter-"), symbol_name));
  Lisp_Object base_name =
    (concat2 (build_pure_c_string ("tree-sitter-"), symbol_name));
  char *c_name = strdup (SSDATA (base_name));
  ts_symbol_to_c_name (c_name);

  /* Override the library name and C name, if appropriate.  */
  Lisp_Object override_name;
  Lisp_Object override_c_name;
  bool found_override = ts_find_override_name
    (language_symbol, &override_name, &override_c_name);
  if (found_override)
    {
      lib_base_name = override_name;
      c_name = SSDATA (override_c_name);
    }

  /* Now we generate a list of possible library paths.  */
  Lisp_Object path_candidates = Qnil;
  /* First push just the filenames to the candidate list, which will
     make dynlib_open look under standard system load paths.  */
  ts_load_language_push_for_each_suffix
    (lib_base_name, &path_candidates);
  /* Then push ~/.emacs.d/tree-sitter paths.  */
  ts_load_language_push_for_each_suffix
    (Fexpand_file_name
     (concat2 (build_string ("tree-sitter/"), lib_base_name),
      Fsymbol_value (Quser_emacs_directory)),
     &path_candidates);
  /* Then push paths from treesit-extra-load-path.  */
  for (Lisp_Object tail = Freverse (Vtreesit_extra_load_path);
       !NILP (tail); tail = XCDR (tail))
    {
      ts_load_language_push_for_each_suffix
	(Fexpand_file_name (lib_base_name, XCAR (tail)),
	 &path_candidates);
    }

  /* Try loading the dynamic library by each path candidate.  Stop
     when succeed, record the error message and try the next one when
     fail.  */
  dynlib_handle_ptr handle;
  char const *error;
  Lisp_Object error_list = Qnil;
  for (Lisp_Object tail = path_candidates;
       !NILP (tail); tail = XCDR (tail))
    {
      char *library_name = SSDATA (XCAR (tail));
      dynlib_error ();
      handle = dynlib_open (library_name);
      error = dynlib_error ();
      if (error == NULL)
	break;
      else
	error_list = Fcons (build_string (error), error_list);
    }
  if (error != NULL)
    {
      *signal_symbol = Qtreesit_load_language_error;
      *signal_data = Fcons (Qnot_found, Fnreverse (error_list));
      return NULL;
    }

  /* Load TSLanguage.  */
  dynlib_error ();
  TSLanguage *(*langfn) (void);
  langfn = dynlib_sym (handle, c_name);
  error = dynlib_error ();
  if (error != NULL)
    {
      *signal_symbol = Qtreesit_load_language_error;
      *signal_data = list2 (Qsymbol_error, build_string (error));
      return NULL;
    }
  TSLanguage *lang = (*langfn) ();

  /* Check if language version matches tree-sitter version.  */
  TSParser *parser = ts_parser_new ();
  bool success = ts_parser_set_language (parser, lang);
  ts_parser_delete (parser);
  if (!success)
    {
      *signal_symbol = Qtreesit_load_language_error;
      *signal_data = list2 (Qversion_mismatch,
			    make_fixnum (ts_language_version (lang)));
      return NULL;
    }
  return lang;
}

DEFUN ("treesit-language-available-p",
       Ftreesit_langauge_available_p,
       Streesit_language_available_p,
       1, 2, 0,
       doc: /* Return non-nil if LANGUAGE exists and is loadable.

If DETAIL is non-nil, return (t . nil) when LANGUAGE is available,
(nil . DATA) when unavailable.  DATA is the signal data of
`treesit-load-language-error'.  */)
  (Lisp_Object language, Lisp_Object detail)
{
  CHECK_SYMBOL (language);
  ts_initialize ();
  Lisp_Object signal_symbol = Qnil;
  Lisp_Object signal_data = Qnil;
  if (ts_load_language(language, &signal_symbol, &signal_data) == NULL)
    {
      if (NILP (detail))
	return Qnil;
      else
	return Fcons (Qnil, signal_data);
    }
  else
    {
      if (NILP (detail))
	return Qt;
      else
	return Fcons (Qt, Qnil);
    }
}

DEFUN ("treesit-language-version",
       Ftreesit_language_version,
       Streesit_language_version,
       0, 1, 0,
       doc: /* Return the language version of tree-sitter library.
If MIN-COMPATIBLE non-nil, return the minimal compatible version.  */)
  (Lisp_Object min_compatible)
{
  if (NILP (min_compatible))
    return make_fixnum (TREE_SITTER_LANGUAGE_VERSION);
  else
    return make_fixnum (TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION);
}

/*** Parsing functions */

static void
ts_check_parser (Lisp_Object obj)
{
  CHECK_TS_PARSER (obj);
  if (XTS_PARSER (obj)->deleted)
    xsignal1 (Qtreesit_parser_deleted, obj);
}

/* An auxiliary function that saves a few lines of code.  Assumes TREE
   is not NULL.  */
static inline void
ts_tree_edit_1 (TSTree *tree, ptrdiff_t start_byte,
		ptrdiff_t old_end_byte, ptrdiff_t new_end_byte)
{
  eassert (start_byte >= 0);
  eassert (start_byte <= old_end_byte);
  eassert (start_byte <= new_end_byte);
  TSPoint dummy_point = {0, 0};
  TSInputEdit edit = {(uint32_t) start_byte,
		      (uint32_t) old_end_byte,
		      (uint32_t) new_end_byte,
		      dummy_point, dummy_point, dummy_point};
  ts_tree_edit (tree, &edit);
}

/* Update each parser's tree after the user made an edit.  This
function does not parse the buffer and only updates the tree. (So it
should be very fast.)  */
void
ts_record_change (ptrdiff_t start_byte, ptrdiff_t old_end_byte,
		  ptrdiff_t new_end_byte)
{
  for (Lisp_Object parser_list
	 = BVAR (current_buffer, ts_parser_list);
       !NILP (parser_list);
       parser_list = XCDR (parser_list))
    {
      CHECK_CONS (parser_list);
      Lisp_Object lisp_parser = XCAR (parser_list);
      ts_check_parser (lisp_parser);
      TSTree *tree = XTS_PARSER (lisp_parser)->tree;
      if (tree != NULL)
	{
	  eassert (start_byte <= old_end_byte);
	  eassert (start_byte <= new_end_byte);
	  /* Think the recorded change as a delete followed by an
	     insert, and think of them as moving unchanged text back
	     and forth.  After all, the whole point of updating the
	     tree is to update the position of unchanged text.  */
	  ptrdiff_t visible_beg = XTS_PARSER (lisp_parser)->visible_beg;
	  ptrdiff_t visible_end = XTS_PARSER (lisp_parser)->visible_end;
	  eassert (visible_beg >= 0);
	  eassert (visible_beg <= visible_end);

	  /* AFFECTED_START/OLD_END/NEW_END are (0-based) offsets from
	     VISIBLE_BEG.  min(visi_end, max(visi_beg, value)) clips
	     value into [visi_beg, visi_end], and subtracting visi_beg
	     gives the offset from visi_beg.  */
	  ptrdiff_t start_offset =
	    min (visible_end,
		 max (visible_beg, start_byte)) - visible_beg;
	  ptrdiff_t old_end_offset =
	    min (visible_end,
		 max (visible_beg, old_end_byte)) - visible_beg;
	  ptrdiff_t new_end_offset =
	    min (visible_end,
		 max (visible_beg, new_end_byte)) - visible_beg;
	  eassert (start_offset <= old_end_offset);
	  eassert (start_offset <= new_end_offset);

	  ts_tree_edit_1 (tree, start_offset, old_end_offset,
			  new_end_offset);
	  XTS_PARSER (lisp_parser)->need_reparse = true;
	  XTS_PARSER (lisp_parser)->timestamp++;

	  /* VISIBLE_BEG/END records tree-sitter's range of view in
	     the buffer.  Ee need to adjust them when tree-sitter's
	     view changes.  */
	  ptrdiff_t visi_beg_delta;
	  if (old_end_byte > new_end_byte)
	    {
	      /* Move backward.  */
	      visi_beg_delta = min (visible_beg, new_end_byte)
		- min (visible_beg, old_end_byte);
	    }
	  else
	    {
	      /* Move forward.  */
	      visi_beg_delta = old_end_byte < visible_beg
		? new_end_byte - old_end_byte : 0;
	    }
	  XTS_PARSER (lisp_parser)->visible_beg
	    = visible_beg + visi_beg_delta;
	  XTS_PARSER (lisp_parser)->visible_end
	    = visible_end + visi_beg_delta
	    + (new_end_offset - old_end_offset);
	  eassert (XTS_PARSER (lisp_parser)->visible_beg >= 0);
	  eassert (XTS_PARSER (lisp_parser)->visible_beg
		   <= XTS_PARSER (lisp_parser)->visible_end);
	}
    }
}

static void
ts_ensure_position_synced (Lisp_Object parser)
{
  TSTree *tree = XTS_PARSER (parser)->tree;

  if (tree == NULL)
    return;

  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);
  ptrdiff_t visible_beg = XTS_PARSER (parser)->visible_beg;
  ptrdiff_t visible_end = XTS_PARSER (parser)->visible_end;
  eassert (0 <= visible_beg);
  eassert (visible_beg <= visible_end);

  /* Before we parse or set ranges, catch up with the narrowing
     situation.  We change visible_beg and visible_end to match
     BUF_BEGV_BYTE and BUF_ZV_BYTE, and inform tree-sitter of the
     change.  We want to move the visible range of tree-sitter to
     match the narrowed range. For example,
     from ________|xxxx|__
     to   |xxxx|__________ */

  /* 1. Make sure visible_beg <= BUF_BEGV_BYTE.  */
  if (visible_beg > BUF_BEGV_BYTE (buffer))
    {
      /* Tree-sitter sees: insert at the beginning. */
      ts_tree_edit_1 (tree, 0, 0, visible_beg - BUF_BEGV_BYTE (buffer));
      visible_beg = BUF_BEGV_BYTE (buffer);
      eassert (visible_beg <= visible_end);
    }
  /* 2. Make sure visible_end = BUF_ZV_BYTE.  */
  if (visible_end < BUF_ZV_BYTE (buffer))
    {
      /* Tree-sitter sees: insert at the end.  */
      ts_tree_edit_1 (tree, visible_end - visible_beg,
		      visible_end - visible_beg,
		      BUF_ZV_BYTE (buffer) - visible_beg);
      visible_end = BUF_ZV_BYTE (buffer);
      eassert (visible_beg <= visible_end);
    }
  else if (visible_end > BUF_ZV_BYTE (buffer))
    {
      /* Tree-sitter sees: delete at the end.  */
      ts_tree_edit_1 (tree, BUF_ZV_BYTE (buffer) - visible_beg,
		      visible_end - visible_beg,
		      BUF_ZV_BYTE (buffer) - visible_beg);
      visible_end = BUF_ZV_BYTE (buffer);
      eassert (visible_beg <= visible_end);
    }
  /* 3. Make sure visible_beg = BUF_BEGV_BYTE.  */
  if (visible_beg < BUF_BEGV_BYTE (buffer))
    {
      /* Tree-sitter sees: delete at the beginning.  */
      ts_tree_edit_1 (tree, 0, BUF_BEGV_BYTE (buffer) - visible_beg, 0);
      visible_beg = BUF_BEGV_BYTE (buffer);
      eassert (visible_beg <= visible_end);
    }
  eassert (0 <= visible_beg);
  eassert (visible_beg <= visible_end);

  XTS_PARSER (parser)->visible_beg = visible_beg;
  XTS_PARSER (parser)->visible_end = visible_end;
}

static void
ts_check_buffer_size (struct buffer *buffer)
{
  ptrdiff_t buffer_size =
    (BUF_Z (buffer) - BUF_BEG (buffer));
  if (buffer_size > UINT32_MAX)
    xsignal2 (Qtreesit_buffer_too_large,
	      build_pure_c_string ("Buffer size larger than 4GB, size:"),
	      make_fixnum (buffer_size));
}

/* Parse the buffer.  We don't parse until we have to. When we have
to, we call this function to parse and update the tree.  */
static void
ts_ensure_parsed (Lisp_Object parser)
{
  if (!XTS_PARSER (parser)->need_reparse)
    return;
  TSParser *ts_parser = XTS_PARSER (parser)->parser;
  TSTree *tree = XTS_PARSER(parser)->tree;
  TSInput input = XTS_PARSER (parser)->input;
  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);
  ts_check_buffer_size (buffer);

  /* Before we parse, catch up with the narrowing situation.  */
  ts_ensure_position_synced (parser);

  TSTree *new_tree = ts_parser_parse(ts_parser, tree, input);
  /* This should be very rare (impossible, really): it only happens
     when 1) language is not set (impossible in Emacs because the user
     has to supply a language to create a parser), 2) parse canceled
     due to timeout (impossible because we don't set a timeout), 3)
     parse canceled due to cancellation flag (impossible because we
     don't set the flag).  (See comments for ts_parser_parse in
     tree_sitter/api.h.)  */
  if (new_tree == NULL)
    {
      Lisp_Object buf;
      XSETBUFFER (buf, buffer);
      xsignal1 (Qtreesit_parse_error, buf);
    }

  if (tree != NULL)
    ts_tree_delete (tree);
  XTS_PARSER (parser)->tree = new_tree;
  XTS_PARSER (parser)->need_reparse = false;
}

/* This is the read function provided to tree-sitter to read from a
   buffer.  It reads one character at a time and automatically skips
   the gap.  */
static const char*
ts_read_buffer (void *parser, uint32_t byte_index,
		TSPoint position, uint32_t *bytes_read)
{
  struct buffer *buffer =
    XBUFFER (((struct Lisp_TS_Parser *) parser)->buffer);
  ptrdiff_t visible_beg = ((struct Lisp_TS_Parser *) parser)->visible_beg;
  ptrdiff_t visible_end = ((struct Lisp_TS_Parser *) parser)->visible_end;
  ptrdiff_t byte_pos = byte_index + visible_beg;
  /* We will make sure visible_beg = BUF_BEGV_BYTE before re-parse (in
     ts_ensure_parsed), so byte_pos will never be smaller than
     BUF_BEG_BYTE.  */
  eassert (visible_beg = BUF_BEGV_BYTE (buffer));
  eassert (visible_end = BUF_ZV_BYTE (buffer));

  /* Read one character.  Tree-sitter wants us to set bytes_read to 0
     if it reads to the end of buffer.  It doesn't say what it wants
     for the return value in that case, so we just give it an empty
     string.  */
  char *beg;
  int len;
  /* This function could run from a user command, so it is better to
     do nothing instead of raising an error. (It was a pain in the a**
     to decrypt mega-if-conditions in Emacs source, so I wrote the two
     branches separately, you are welcome.)  */
  if (!BUFFER_LIVE_P (buffer))
    {
      beg = NULL;
      len = 0;
    }
  /* Reached visible end-of-buffer, tell tree-sitter to read no more.  */
  else if (byte_pos >= visible_end)
    {
      beg = NULL;
      len = 0;
    }
  /* Normal case, read a character.  */
  else
    {
      beg = (char *) BUF_BYTE_ADDRESS (buffer, byte_pos);
      len = BYTES_BY_CHAR_HEAD ((int) *beg);
    }
  *bytes_read = (uint32_t) len;
  return beg;
}

/*** Functions for parser and node object*/

/* Wrap the parser in a Lisp_Object to be used in the Lisp machine.  */
Lisp_Object
make_ts_parser (Lisp_Object buffer, TSParser *parser,
		TSTree *tree, Lisp_Object language_symbol)
{
  struct Lisp_TS_Parser *lisp_parser
    = ALLOCATE_PSEUDOVECTOR
    (struct Lisp_TS_Parser, buffer, PVEC_TS_PARSER);

  lisp_parser->language_symbol = language_symbol;
  lisp_parser->buffer = buffer;
  lisp_parser->parser = parser;
  lisp_parser->tree = tree;
  TSInput input = {lisp_parser, ts_read_buffer, TSInputEncodingUTF8};
  lisp_parser->input = input;
  lisp_parser->need_reparse = true;
  lisp_parser->visible_beg = BUF_BEGV (XBUFFER (buffer));
  lisp_parser->visible_end = BUF_ZV (XBUFFER (buffer));
  lisp_parser->timestamp = 0;
  lisp_parser->deleted = false;
  eassert (lisp_parser->visible_beg <= lisp_parser->visible_end);
  return make_lisp_ptr (lisp_parser, Lisp_Vectorlike);
}

/* Wrap the node in a Lisp_Object to be used in the Lisp machine.  */
Lisp_Object
make_ts_node (Lisp_Object parser, TSNode node)
{
  struct Lisp_TS_Node *lisp_node
    = ALLOCATE_PSEUDOVECTOR (struct Lisp_TS_Node, parser, PVEC_TS_NODE);
  lisp_node->parser = parser;
  lisp_node->node = node;
  lisp_node->timestamp = XTS_PARSER (parser)->timestamp;
  return make_lisp_ptr (lisp_node, Lisp_Vectorlike);
}

/* Make a compiled query.  QUERY has to be either a cons or a
   string.  */
static Lisp_Object
make_ts_query (Lisp_Object query, Lisp_Object language)
{
  TSQueryCursor *ts_cursor = ts_query_cursor_new ();
  struct Lisp_TS_Query *lisp_query
    = ALLOCATE_PSEUDOVECTOR (struct Lisp_TS_Query, source,
			     PVEC_TS_COMPILED_QUERY);

  lisp_query->language = language;
  lisp_query->source = query;
  lisp_query->query = NULL;
  lisp_query->cursor = ts_cursor;
  return make_lisp_ptr (lisp_query, Lisp_Vectorlike);
}

static const char*
ts_query_error_to_string (TSQueryError error)
{
  switch (error)
    {
    case TSQueryErrorNone:
      return "None";
    case TSQueryErrorSyntax:
      return "Syntax error at";
    case TSQueryErrorNodeType:
      return "Node type error at";
    case TSQueryErrorField:
      return "Field error at";
    case TSQueryErrorCapture:
      return "Capture error at";
    case TSQueryErrorStructure:
      return "Structure error at";
    default:
      return "Unknown error";
    }
}

static Lisp_Object
ts_compose_query_signal_data
(uint32_t error_offset, TSQueryError error_type)
{
  return list3 (build_string
		(ts_query_error_to_string (error_type)),
		make_fixnum (error_offset + 1),
		build_pure_c_string("Debug the query with `treesit-query-validate'"));
}

/* Ensure the QUERY is compiled.  Return the TSQuery.  It could be
   NULL if error occurs, in which case ERROR_OFFSET and ERROR_TYPE are
   bound.  If error occures, return NULL, and assign SIGNAL_SYMBOL and
   SIGNAL_DATA accordingly.  */
static TSQuery *
ts_ensure_query_compiled
(Lisp_Object query, Lisp_Object *signal_symbol, Lisp_Object *signal_data)
{
  /* If query is already compiled (not null), return that, otherwise
     compile and return it.  */
  TSQuery *ts_query = XTS_COMPILED_QUERY (query)->query;
  if (ts_query != NULL)
    return ts_query;

  /* Get query source and TSLanguage ready.  */
  Lisp_Object source = XTS_COMPILED_QUERY (query)->source;
  Lisp_Object language = XTS_COMPILED_QUERY (query)->language;
  /* This is the main reason why we compile query lazily: to avoid
     loading languages early.  */
  TSLanguage *ts_lang = ts_load_language (language, signal_symbol,
					  signal_data);
  if (ts_lang == NULL)
    return NULL;

  if (CONSP (source))
    source = Ftreesit_query_expand (source);

  /* Create TSQuery.  */
  uint32_t error_offset;
  TSQueryError error_type;
  char *ts_source = SSDATA (source);
  ts_query = ts_query_new (ts_lang, ts_source, strlen (ts_source),
			   &error_offset, &error_type);
  if (ts_query == NULL)
    {
      *signal_symbol = Qtreesit_query_error;
      *signal_data = ts_compose_query_signal_data
	(error_offset, error_type);
    }
  XTS_COMPILED_QUERY (query)->query = ts_query;
  return ts_query;
}

DEFUN ("treesit-parser-p",
       Ftreesit_parser_p, Streesit_parser_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a tree-sitter parser.  */)
  (Lisp_Object object)
{
  if (TS_PARSERP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("treesit-node-p",
       Ftreesit_node_p, Streesit_node_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a tree-sitter node.  */)
  (Lisp_Object object)
{
  if (TS_NODEP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("treesit-compiled-query-p",
       Ftreesit_compiled_query_p, Streesit_compiled_query_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a compiled tree-sitter query.  */)
  (Lisp_Object object)
{
  if (TS_COMPILED_QUERY_P (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("treesit-query-p",
       Ftreesit_query_p, Streesit_query_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a generic tree-sitter query.  */)
  (Lisp_Object object)
{
  if (TS_COMPILED_QUERY_P (object)
      || CONSP (object) || STRINGP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("treesit-query-language",
       Ftreesit_query_language, Streesit_query_language, 1, 1, 0,
       doc: /* Return the language of QUERY.
QUERY has to be a compiled query.  */)
  (Lisp_Object query)
{
  CHECK_TS_COMPILED_QUERY (query);
  return XTS_COMPILED_QUERY(query)->language;
}

DEFUN ("treesit-node-parser",
       Ftreesit_node_parser, Streesit_node_parser,
       1, 1, 0,
       doc: /* Return the parser to which NODE belongs.  */)
  (Lisp_Object node)
{
  CHECK_TS_NODE (node);
  return XTS_NODE (node)->parser;
}

DEFUN ("treesit-parser-create",
       Ftreesit_parser_create, Streesit_parser_create,
       1, 3, 0,
       doc: /* Create and return a parser in BUFFER for LANGUAGE.

The parser is automatically added to BUFFER's `treesit-parser-list'.
LANGUAGE is a language symbol.  If BUFFER is nil, use the current
buffer.  If BUFFER already has a parser for LANGUAGE, return that
parser.  If NO-REUSE is non-nil, always create a new parser.  */)
  (Lisp_Object language, Lisp_Object buffer, Lisp_Object no_reuse)
{
  ts_initialize ();

  CHECK_SYMBOL (language);
  struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }
  ts_check_buffer_size (buf);

  /* See if we can reuse a parser.  */
  for (Lisp_Object tail = BVAR (buf, ts_parser_list);
       NILP (no_reuse) && !NILP (tail);
       tail = XCDR (tail))
    {
      struct Lisp_TS_Parser *parser = XTS_PARSER (XCAR (tail));
      if (EQ (parser->language_symbol, language))
	{
	  return XCAR (tail);
	}
    }

  /* Load language.  */
  Lisp_Object signal_symbol = Qnil;
  Lisp_Object signal_data = Qnil;
  TSParser *parser = ts_parser_new ();
  TSLanguage *lang = ts_load_language (language, &signal_symbol,
				       &signal_data);
  if (lang == NULL)
    xsignal (signal_symbol, signal_data);
  /* We check language version when loading a language, so this should
     always succeed.  */
  ts_parser_set_language (parser, lang);

  /* Create parser.  */
  Lisp_Object lisp_parser
    = make_ts_parser (Fcurrent_buffer (), parser, NULL, language);

  /* Update parser-list.  */
  BVAR (buf, ts_parser_list)
    = Fcons (lisp_parser, BVAR (buf, ts_parser_list));

  return lisp_parser;
}

DEFUN ("treesit-parser-delete",
       Ftreesit_parser_delete, Streesit_parser_delete,
       1, 1, 0,
       doc: /* Delete PARSER from its buffer.  */)
  (Lisp_Object parser)
{
  ts_check_parser (parser);

  Lisp_Object buffer = XTS_PARSER (parser)->buffer;
  struct buffer *buf = XBUFFER (buffer);
  BVAR (buf, ts_parser_list)
    = Fdelete (parser, BVAR (buf, ts_parser_list));

  XTS_PARSER (parser)->deleted = true;
  return Qnil;
}

DEFUN ("treesit-parser-list",
       Ftreesit_parser_list, Streesit_parser_list,
       0, 1, 0,
       doc: /* Return BUFFER's parser list.
BUFFER defaults to the current buffer.  */)
  (Lisp_Object buffer)
{
  struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }
  /* Return a fresh list so messing with that list doesn't affect our
     internal data.  */
  Lisp_Object return_list = Qnil;
  for (Lisp_Object tail = BVAR (buf, ts_parser_list);
       !NILP (tail);
       tail = XCDR (tail))
    {
      return_list = Fcons (XCAR (tail), return_list);
    }
  return Freverse (return_list);
}

DEFUN ("treesit-parser-buffer",
       Ftreesit_parser_buffer, Streesit_parser_buffer,
       1, 1, 0,
       doc: /* Return the buffer of PARSER.  */)
  (Lisp_Object parser)
{
  ts_check_parser (parser);
  Lisp_Object buf;
  XSETBUFFER (buf, XBUFFER (XTS_PARSER (parser)->buffer));
  return buf;
}

DEFUN ("treesit-parser-language",
       Ftreesit_parser_language, Streesit_parser_language,
       1, 1, 0,
       doc: /* Return parser's language symbol.
This symbol is the one used to create the parser.  */)
  (Lisp_Object parser)
{
  ts_check_parser (parser);
  return XTS_PARSER (parser)->language_symbol;
}

/*** Parser API */

DEFUN ("treesit-parser-root-node",
       Ftreesit_parser_root_node, Streesit_parser_root_node,
       1, 1, 0,
       doc: /* Return the root node of PARSER.  */)
  (Lisp_Object parser)
{
  ts_check_parser (parser);
  ts_ensure_parsed (parser);
  TSNode root_node = ts_tree_root_node (XTS_PARSER (parser)->tree);
  return make_ts_node (parser, root_node);
}

/* Checks that the RANGES argument of
   treesit-parser-set-included-ranges is valid.  */
static void
ts_check_range_argument (Lisp_Object ranges)
{
  struct buffer *buffer = current_buffer;
  ptrdiff_t point_min = BUF_BEGV (buffer);
  ptrdiff_t point_max = BUF_ZV (buffer);
  EMACS_INT last_point = point_min;

  for (Lisp_Object tail = ranges;
       !NILP (tail); tail = XCDR (tail))
    {
      CHECK_CONS (tail);
      Lisp_Object range = XCAR (tail);
      CHECK_CONS (range);
      CHECK_FIXNUM (XCAR (range));
      CHECK_FIXNUM (XCDR (range));
      EMACS_INT beg = XFIXNUM (XCAR (range));
      EMACS_INT end = XFIXNUM (XCDR (range));
      if (!(last_point <= beg && beg <= end && end <= point_max))
	xsignal2 (Qtreesit_range_invalid,
		  build_pure_c_string
		  ("RANGE is either overlapping or out-of-order or out-of-range"),
		  ranges);
      last_point = end;
    }
}

DEFUN ("treesit-parser-set-included-ranges",
       Ftreesit_parser_set_included_ranges,
       Streesit_parser_set_included_ranges,
       2, 2, 0,
       doc: /* Limit PARSER to RANGES.

RANGES is a list of (BEG . END), each (BEG . END) confines a range in
which the parser should operate in.  Each range must not overlap, and
each range should come in order.  Signal `treesit-set-range-error'
if the argument is invalid, or something else went wrong.  If RANGES
is nil, set PARSER to parse the whole buffer.  */)
  (Lisp_Object parser, Lisp_Object ranges)
{
  ts_check_parser (parser);
  CHECK_CONS (ranges);
  ts_check_range_argument (ranges);

  /* Before we parse, catch up with narrowing/widening.  */
  ts_ensure_position_synced (parser);

  bool success;
  if (NILP (ranges))
    {
      /* If RANGES is nil, make parser to parse the whole document.
	 To do that we give tree-sitter a 0 length, the range is a
	 dummy.  */
      TSRange ts_range = {{0, 0}, {0, 0}, 0, 0};
      success = ts_parser_set_included_ranges
	(XTS_PARSER (parser)->parser, &ts_range , 0);
    }
  else
    {
      /* Set ranges for PARSER.  */
      ptrdiff_t len = list_length (ranges);
      TSRange *ts_ranges = malloc (sizeof(TSRange) * len);
      struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);

      for (int idx=0; !NILP (ranges); idx++, ranges = XCDR (ranges))
	{
	  Lisp_Object range = XCAR (ranges);
	  EMACS_INT beg_byte = buf_charpos_to_bytepos
	    (buffer, XFIXNUM (XCAR (range)));
	  EMACS_INT end_byte = buf_charpos_to_bytepos
	    (buffer, XFIXNUM (XCDR (range)));
	  /* We don't care about start and end points, put in dummy
	     value.  */
	  TSRange rg = {{0,0}, {0,0},
			(uint32_t) beg_byte - BUF_BEGV_BYTE (buffer),
			(uint32_t) end_byte - BUF_BEGV_BYTE (buffer)};
	  ts_ranges[idx] = rg;
	}
      success = ts_parser_set_included_ranges
	(XTS_PARSER (parser)->parser, ts_ranges, (uint32_t) len);
      /* Although XFIXNUM could signal, it should be impossible
	 because we have checked the input by ts_check_range_argument.
	 So there is no need for unwind-protect.  */
      free (ts_ranges);
    }

  if (!success)
    xsignal2 (Qtreesit_range_invalid,
	      build_pure_c_string
	      ("Something went wrong when setting ranges"),
	      ranges);

  XTS_PARSER (parser)->need_reparse = true;
  return Qnil;
}

DEFUN ("treesit-parser-included-ranges",
       Ftreesit_parser_included_ranges,
       Streesit_parser_included_ranges,
       1, 1, 0,
       doc: /* Return the ranges set for PARSER.
See `treesit-parser-set-ranges'.  If no range is set, return
nil.  */)
  (Lisp_Object parser)
{
  ts_check_parser (parser);
  uint32_t len;
  const TSRange *ranges = ts_parser_included_ranges
    (XTS_PARSER (parser)->parser, &len);
  if (len == 0)
    return Qnil;

  /* Our return value depends on the buffer state (BUF_BEGV_BYTE,
     etc), so we need to sync up.  */
  ts_ensure_position_synced (parser);

  struct buffer *buffer = XBUFFER (XTS_PARSER (parser)->buffer);

  Lisp_Object list = Qnil;
  for (int idx=0; idx < len; idx++)
    {
      TSRange range = ranges[idx];
      uint32_t beg_byte = range.start_byte + BUF_BEGV_BYTE (buffer);
      uint32_t end_byte = range.end_byte + BUF_BEGV_BYTE (buffer);
      eassert (BUF_BEGV_BYTE (buffer) <= beg_byte);
      eassert (beg_byte <= end_byte);
      eassert (end_byte <= BUF_ZV_BYTE (buffer));

      Lisp_Object lisp_range =
	Fcons (make_fixnum (buf_bytepos_to_charpos (buffer, beg_byte)) ,
	       make_fixnum (buf_bytepos_to_charpos (buffer, end_byte)));
      list = Fcons (lisp_range, list);
    }
  return Fnreverse (list);
}

/*** Node API  */

/* Check that OBJ is a positive integer and signal an error if
   otherwise. */
static void
ts_check_positive_integer (Lisp_Object obj)
{
  CHECK_INTEGER (obj);
  if (XFIXNUM (obj) < 0)
    xsignal1 (Qargs_out_of_range, obj);
}

static void
ts_check_node (Lisp_Object obj)
{
  CHECK_TS_NODE (obj);
  Lisp_Object lisp_parser = XTS_NODE (obj)->parser;
  if (XTS_NODE (obj)->timestamp !=
      XTS_PARSER (lisp_parser)->timestamp)
    xsignal1 (Qtreesit_node_outdated, obj);
}

DEFUN ("treesit-node-type",
       Ftreesit_node_type, Streesit_node_type, 1, 1, 0,
       doc: /* Return the NODE's type as a string.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  TSNode ts_node = XTS_NODE (node)->node;
  const char *type = ts_node_type (ts_node);
  return build_string (type);
}

DEFUN ("treesit-node-start",
       Ftreesit_node_start, Streesit_node_start, 1, 1, 0,
       doc: /* Return the NODE's start position.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  TSNode ts_node = XTS_NODE (node)->node;
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  uint32_t start_byte_offset = ts_node_start_byte (ts_node);
  struct buffer *buffer =
    XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t start_pos = buf_bytepos_to_charpos
    (buffer, start_byte_offset + visible_beg);
  return make_fixnum (start_pos);
}

DEFUN ("treesit-node-end",
       Ftreesit_node_end, Streesit_node_end, 1, 1, 0,
       doc: /* Return the NODE's end position.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  TSNode ts_node = XTS_NODE (node)->node;
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  uint32_t end_byte_offset = ts_node_end_byte (ts_node);
  struct buffer *buffer =
    XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t end_pos = buf_bytepos_to_charpos
    (buffer, end_byte_offset + visible_beg);
  return make_fixnum (end_pos);
}

DEFUN ("treesit-node-string",
       Ftreesit_node_string, Streesit_node_string, 1, 1, 0,
       doc: /* Return the string representation of NODE.
If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  TSNode ts_node = XTS_NODE (node)->node;
  char *string = ts_node_string (ts_node);
  return build_string (string);
}

DEFUN ("treesit-node-parent",
       Ftreesit_node_parent, Streesit_node_parent, 1, 1, 0,
       doc: /* Return the immediate parent of NODE.
Return nil if there isn't any.  If NODE is nil, return nil.  */)
  (Lisp_Object node)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode parent = ts_node_parent (ts_node);

  if (ts_node_is_null (parent))
    return Qnil;

  return make_ts_node (XTS_NODE (node)->parser, parent);
}

DEFUN ("treesit-node-child",
       Ftreesit_node_child, Streesit_node_child, 2, 3, 0,
       doc: /* Return the Nth child of NODE.

Return nil if there isn't any.  If NAMED is non-nil, look for named
child only.  NAMED defaults to nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object n, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  ts_check_positive_integer (n);
  EMACS_INT idx = XFIXNUM (n);
  if (idx > UINT32_MAX) xsignal1 (Qargs_out_of_range, n);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode child;
  if (NILP (named))
    child = ts_node_child (ts_node, (uint32_t) idx);
  else
    child = ts_node_named_child (ts_node, (uint32_t) idx);

  if (ts_node_is_null (child))
    return Qnil;

  return make_ts_node (XTS_NODE (node)->parser, child);
}

DEFUN ("treesit-node-check",
       Ftreesit_node_check, Streesit_node_check, 2, 2, 0,
       doc: /* Return non-nil if NODE has PROPERTY, nil otherwise.

PROPERTY could be 'named, 'missing, 'extra, 'has-changes, 'has-error.
Named nodes correspond to named rules in the language definition,
whereas "anonymous" nodes correspond to string literals in the
language definition.

Missing nodes are inserted by the parser in order to recover from
certain kinds of syntax errors, i.e., should be there but not there.

Extra nodes represent things like comments, which are not required the
language definition, but can appear anywhere.

A node "has changes" if the buffer changed since the node is
created. (Don't forget the "s" at the end of 'has-changes.)

A node "has error" if itself is a syntax error or contains any syntax
errors.  */)
  (Lisp_Object node, Lisp_Object property)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  CHECK_SYMBOL (property);
  TSNode ts_node = XTS_NODE (node)->node;
  bool result;
  if (EQ (property, Qnamed))
    result = ts_node_is_named (ts_node);
  else if (EQ (property, Qmissing))
    result = ts_node_is_missing (ts_node);
  else if (EQ (property, Qextra))
    result = ts_node_is_extra (ts_node);
  else if (EQ (property, Qhas_error))
    result = ts_node_has_error (ts_node);
  else if (EQ (property, Qhas_changes))
    result = ts_node_has_changes (ts_node);
  else
    signal_error ("Expecting 'named, 'missing, 'extra, 'has-changes or 'has-error, got",
		  property);
  return result ? Qt : Qnil;
}

DEFUN ("treesit-node-field-name-for-child",
       Ftreesit_node_field_name_for_child,
       Streesit_node_field_name_for_child, 2, 2, 0,
       doc: /* Return the field name of the Nth child of NODE.

Return nil if not any child or no field is found.
If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object n)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  ts_check_positive_integer (n);
  EMACS_INT idx = XFIXNUM (n);
  if (idx > UINT32_MAX) xsignal1 (Qargs_out_of_range, n);
  TSNode ts_node = XTS_NODE (node)->node;
  const char *name
    = ts_node_field_name_for_child (ts_node, (uint32_t) idx);

  if (name == NULL)
    return Qnil;

  return build_string (name);
}

DEFUN ("treesit-node-child-count",
       Ftreesit_node_child_count,
       Streesit_node_child_count, 1, 2, 0,
       doc: /* Return the number of children of NODE.

If NAMED is non-nil, count named child only.  NAMED defaults to
nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  TSNode ts_node = XTS_NODE (node)->node;
  uint32_t count;
  if (NILP (named))
    count = ts_node_child_count (ts_node);
  else
    count = ts_node_named_child_count (ts_node);
  return make_fixnum (count);
}

DEFUN ("treesit-node-child-by-field-name",
       Ftreesit_node_child_by_field_name,
       Streesit_node_child_by_field_name, 2, 2, 0,
       doc: /* Return the child of NODE with FIELD-NAME.
Return nil if there isn't any.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object field_name)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  CHECK_STRING (field_name);
  char *name_str = SSDATA (field_name);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode child
    = ts_node_child_by_field_name (ts_node, name_str, strlen (name_str));

  if (ts_node_is_null(child))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, child);
}

DEFUN ("treesit-node-next-sibling",
       Ftreesit_node_next_sibling,
       Streesit_node_next_sibling, 1, 2, 0,
       doc: /* Return the next sibling of NODE.

Return nil if there isn't any.  If NAMED is non-nil, look for named
child only.  NAMED defaults to nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode sibling;
  if (NILP (named))
    sibling = ts_node_next_sibling (ts_node);
  else
    sibling = ts_node_next_named_sibling (ts_node);

  if (ts_node_is_null(sibling))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, sibling);
}

DEFUN ("treesit-node-prev-sibling",
       Ftreesit_node_prev_sibling,
       Streesit_node_prev_sibling, 1, 2, 0,
       doc: /* Return the previous sibling of NODE.

Return nil if there isn't any.  If NAMED is non-nil, look for named
child only.  NAMED defaults to nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  TSNode ts_node = XTS_NODE (node)->node;
  TSNode sibling;

  if (NILP (named))
    sibling = ts_node_prev_sibling (ts_node);
  else
    sibling = ts_node_prev_named_sibling (ts_node);

  if (ts_node_is_null(sibling))
    return Qnil;

  return make_ts_node(XTS_NODE (node)->parser, sibling);
}

DEFUN ("treesit-node-first-child-for-pos",
       Ftreesit_node_first_child_for_pos,
       Streesit_node_first_child_for_pos, 2, 3, 0,
       doc: /* Return the first child of NODE on POS.

Specifically, return the first child that extends beyond POS.  POS is
a position in the buffer.  Return nil if there isn't any.  If NAMED is
non-nil, look for named child only.  NAMED defaults to nil.  Note that
this function returns an immediate child, not the smallest
(grand)child.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object pos, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  ts_check_positive_integer (pos);

  struct buffer *buf =
    XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  ptrdiff_t byte_pos = buf_charpos_to_bytepos (buf, XFIXNUM (pos));

  if (byte_pos < BUF_BEGV_BYTE (buf) || byte_pos > BUF_ZV_BYTE (buf))
    xsignal1 (Qargs_out_of_range, pos);

  TSNode ts_node = XTS_NODE (node)->node;
  TSNode child;
  if (NILP (named))
    child = ts_node_first_child_for_byte
      (ts_node, byte_pos - visible_beg);
  else
    child = ts_node_first_named_child_for_byte
      (ts_node, byte_pos - visible_beg);

  if (ts_node_is_null (child))
    return Qnil;

  return make_ts_node (XTS_NODE (node)->parser, child);
}

DEFUN ("treesit-node-descendant-for-range",
       Ftreesit_node_descendant_for_range,
       Streesit_node_descendant_for_range, 3, 4, 0,
       doc: /* Return the smallest node that covers BEG to END.

The returned node is a descendant of NODE.  POS is a position.  Return
nil if there isn't any.  If NAMED is non-nil, look for named child
only.  NAMED defaults to nil.  If NODE is nil, return nil.  */)
  (Lisp_Object node, Lisp_Object beg, Lisp_Object end, Lisp_Object named)
{
  if (NILP (node)) return Qnil;
  ts_check_node (node);
  CHECK_INTEGER (beg);
  CHECK_INTEGER (end);

  struct buffer *buf =
    XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer);
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (node)->parser)->visible_beg;
  ptrdiff_t byte_beg = buf_charpos_to_bytepos (buf, XFIXNUM (beg));
  ptrdiff_t byte_end = buf_charpos_to_bytepos (buf, XFIXNUM (end));

  /* Checks for BUFFER_BEG <= BEG <= END <= BUFFER_END.  */
  if (!(BUF_BEGV_BYTE (buf) <= byte_beg
	&& byte_beg <= byte_end
	&& byte_end <= BUF_ZV_BYTE (buf)))
    xsignal2 (Qargs_out_of_range, beg, end);

  TSNode ts_node = XTS_NODE (node)->node;
  TSNode child;
  if (NILP (named))
    child = ts_node_descendant_for_byte_range
      (ts_node, byte_beg - visible_beg , byte_end - visible_beg);
  else
    child = ts_node_named_descendant_for_byte_range
      (ts_node, byte_beg - visible_beg, byte_end - visible_beg);

  if (ts_node_is_null (child))
    return Qnil;

  return make_ts_node (XTS_NODE (node)->parser, child);
}

DEFUN ("treesit-node-eq",
       Ftreesit_node_eq,
       Streesit_node_eq, 2, 2, 0,
       doc: /* Return non-nil if NODE1 and NODE2 are the same node.
If any one of NODE1 and NODE2 is nil, return nil.  */)
  (Lisp_Object node1, Lisp_Object node2)
{
  if (NILP (node1) || NILP (node2))
    return Qnil;
  CHECK_TS_NODE (node1);
  CHECK_TS_NODE (node2);

  TSNode ts_node_1 = XTS_NODE (node1)->node;
  TSNode ts_node_2 = XTS_NODE (node2)->node;

  bool same_node = ts_node_eq (ts_node_1, ts_node_2);
  return same_node ? Qt : Qnil;
}

/*** Query functions */

DEFUN ("treesit-pattern-expand",
       Ftreesit_pattern_expand,
       Streesit_pattern_expand, 1, 1, 0,
       doc: /* Expand PATTERN to its string form.

PATTERN can be

    :anchor
    :?
    :*
    :+
    :equal
    :match
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

Consult Info node `(elisp)Pattern Matching' form detailed
explanation.  */)
  (Lisp_Object pattern)
{
  if (EQ (pattern, intern_c_string (":anchor")))
    return build_pure_c_string(".");
  if (EQ (pattern, intern_c_string (":?")))
    return build_pure_c_string("?");
  if (EQ (pattern, intern_c_string (":*")))
    return build_pure_c_string("*");
  if (EQ (pattern, intern_c_string (":+")))
    return build_pure_c_string("+");
  if (EQ (pattern, intern_c_string (":equal")))
    return build_pure_c_string("#equal");
  if (EQ (pattern, intern_c_string (":match")))
    return build_pure_c_string("#match");
  Lisp_Object opening_delimeter =
    build_pure_c_string (VECTORP (pattern) ? "[" : "(");
  Lisp_Object closing_delimiter =
    build_pure_c_string (VECTORP (pattern) ? "]" : ")");
  if (VECTORP (pattern) || CONSP (pattern))
    return concat3 (opening_delimeter,
		    Fmapconcat (intern_c_string
				("treesit-pattern-expand"),
				pattern,
				build_pure_c_string (" ")),
		    closing_delimiter);
  return CALLN (Fformat, build_pure_c_string("%S"), pattern);
}

DEFUN ("treesit-query-expand",
       Ftreesit_query_expand,
       Streesit_query_expand, 1, 1, 0,
       doc: /* Expand sexp QUERY to its string form.

A PATTERN in QUERY can be

    :anchor
    :?
    :*
    :+
    :equal
    :match
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

Consult Info node `(elisp)Pattern Matching' form detailed
explanation.  */)
  (Lisp_Object query)
{
  return Fmapconcat (intern_c_string ("treesit-pattern-expand"),
		     query, build_pure_c_string (" "));
}

/* This struct is used for passing captures to be check against
   predicates.  Captures we check for are the ones in START before
   END.  For example, if START and END are

   START       END
    v              v
   (1 . (2 . (3 . (4 . (5 . (6 . nil))))))

   We only look at captures 1 2 3.  */
struct capture_range
{
  Lisp_Object start;
  Lisp_Object end;
};

/* Collect predicates for this match and return them in a list.  Each
   predicate is a list of strings and symbols.  */
static Lisp_Object
ts_predicates_for_pattern
(TSQuery *query, uint32_t pattern_index)
{
  uint32_t len;
  const TSQueryPredicateStep *predicate_list =
    ts_query_predicates_for_pattern (query, pattern_index, &len);
  Lisp_Object result = Qnil;
  Lisp_Object predicate = Qnil;
  for (int idx=0; idx < len; idx++)
    {
      TSQueryPredicateStep step = predicate_list[idx];
      switch (step.type)
	{
	case TSQueryPredicateStepTypeCapture:
	  {
	    uint32_t str_len;
	    const char *str = ts_query_capture_name_for_id
	      (query, step.value_id, &str_len);
	    predicate = Fcons (intern_c_string_1 (str, str_len),
			       predicate);
	    break;
	  }
	case TSQueryPredicateStepTypeString:
	  {
	    uint32_t str_len;
	    const char *str = ts_query_string_value_for_id
	      (query, step.value_id, &str_len);
	    predicate = Fcons (make_string (str, str_len), predicate);
	    break;
	  }
	case TSQueryPredicateStepTypeDone:
	  result = Fcons (Fnreverse (predicate), result);
	  predicate = Qnil;
	  break;
	}
    }
  return Fnreverse (result);
}

/* Translate a capture NAME (symbol) to the text of the captured node.
   Signals treesit-query-error if such node is not captured.  */
static Lisp_Object
ts_predicate_capture_name_to_text
(Lisp_Object name, struct capture_range captures)
{
  Lisp_Object node = Qnil;
  for (Lisp_Object tail = captures.start;
       !EQ (tail, captures.end); tail = XCDR (tail))
    {
      if (EQ (XCAR (XCAR (tail)), name))
	{
	  node = XCDR (XCAR (tail));
	  break;
	}
    }

  if (NILP (node))
    xsignal3 (Qtreesit_query_error,
	      build_pure_c_string ("Cannot find captured node"),
	      name, build_pure_c_string ("A predicate can only refer to captured nodes in the same pattern"));

  struct buffer *old_buffer = current_buffer;
  set_buffer_internal
    (XBUFFER (XTS_PARSER (XTS_NODE (node)->parser)->buffer));
  Lisp_Object text = Fbuffer_substring
    (Ftreesit_node_start (node), Ftreesit_node_end (node));
  set_buffer_internal (old_buffer);
  return text;
}

/* Handles predicate (#equal A B).  Return true if A equals B; return
   false otherwise. A and B can be either string, or a capture name.
   The capture name evaluates to the text its captured node spans in
   the buffer.  */
static bool
ts_predicate_equal
(Lisp_Object args, struct capture_range captures)
{
  if (XFIXNUM (Flength (args)) != 2)
    xsignal2 (Qtreesit_query_error, build_pure_c_string ("Predicate `equal' requires two arguments but only given"), Flength (args));

  Lisp_Object arg1 = XCAR (args);
  Lisp_Object arg2 = XCAR (XCDR (args));
  Lisp_Object text1 = STRINGP (arg1) ? arg1 :
    ts_predicate_capture_name_to_text (arg1, captures);
  Lisp_Object text2 = STRINGP (arg2) ? arg2 :
    ts_predicate_capture_name_to_text (arg2, captures);

  if (NILP (Fstring_equal (text1, text2)))
    return false;
  else
    return true;
}

/* Handles predicate (#match "regexp" @node).  Return true if "regexp"
   matches the text spanned by @node; return false otherwise.  Matching
   is case-sensitive.  */
static bool
ts_predicate_match
(Lisp_Object args, struct capture_range captures)
{
  if (XFIXNUM (Flength (args)) != 2)
    xsignal2 (Qtreesit_query_error, build_pure_c_string ("Predicate `equal' requires two arguments but only given"), Flength (args));

  Lisp_Object regexp = XCAR (args);
  Lisp_Object capture_name = XCAR (XCDR (args));
  Lisp_Object text = ts_predicate_capture_name_to_text
    (capture_name, captures);

  /* It's probably common to get the argument order backwards.  Catch
     this mistake early and show helpful explanation, because Emacs
     loves you.  (We put the regexp first because that's what
     string-match does.)  */
  if (!STRINGP (regexp))
    xsignal1 (Qtreesit_query_error, build_pure_c_string ("The first argument to `match' should be a regexp string, not a capture name"));
  if (!SYMBOLP (capture_name))
    xsignal1 (Qtreesit_query_error, build_pure_c_string ("The second argument to `match' should be a capture name, not a string"));

  if (fast_string_match (regexp, text) >= 0)
    return true;
  else
    return false;
}

/* About predicates: I decide to hard-code predicates in C instead of
   implementing an extensible system where predicates are translated
   to Lisp functions, and new predicates can be added by extending a
   list of functions, because I really couldn't imagine any useful
   predicates besides equal and match.  If we later found out that
   such system is indeed useful and necessary, it can be easily
   added.  */

/* If all predicates in PREDICATES passes, return true; otherwise
   return false.  */
static bool
ts_eval_predicates
(struct capture_range captures, Lisp_Object predicates)
{
  bool pass = true;
  /* Evaluate each predicates.  */
  for (Lisp_Object tail = predicates;
       !NILP (tail); tail = XCDR (tail))
    {
      Lisp_Object predicate = XCAR (tail);
      Lisp_Object fn = XCAR (predicate);
      Lisp_Object args = XCDR (predicate);
      if (!NILP (Fstring_equal (fn, build_pure_c_string("equal"))))
	pass = ts_predicate_equal (args, captures);
      else if (!NILP (Fstring_equal
		      (fn, build_pure_c_string("match"))))
	pass = ts_predicate_match (args, captures);
      else
	xsignal3 (Qtreesit_query_error,
		  build_pure_c_string ("Invalid predicate"),
		  fn, build_pure_c_string ("Currently Emacs only supports equal and match predicate"));
    }
  /* If all predicates passed, add captures to result list.  */
  return pass;
}

DEFUN ("treesit-query-compile",
       Ftreesit_query_compile,
       Streesit_query_compile, 2, 3, 0,
       doc: /* Compile QUERY to a compiled query.

Querying a compiled query is much faster than an uncompiled one.
LANGUAGE is the language this query is for.

If EAGER is non-nil, immediately load LANGUAGE and compile the query.
Otherwise defer until the query is first used.

Signals treesit-query-error if QUERY is malformed or something else
goes wrong.  (This of course would only happen if EAGER is non-nil.)
You can use `treesit-query-validate' to debug the query.  */)
  (Lisp_Object language, Lisp_Object query, Lisp_Object eager)
{
  if (NILP (Ftreesit_query_p (query)))
    wrong_type_argument (Qtreesit_query_p, query);
  CHECK_SYMBOL (language);
  if (TS_COMPILED_QUERY_P (query))
    return query;

  Lisp_Object lisp_query = make_ts_query (query, language);

  /* Maybe actually compile. */
  if (NILP (eager))
      return lisp_query;
  else
    {
      Lisp_Object signal_symbol = Qnil;
      Lisp_Object signal_data = Qnil;
      TSQuery *ts_query = ts_ensure_query_compiled
	(lisp_query, &signal_symbol, &signal_data);

      if (ts_query == NULL)
	xsignal (signal_symbol, signal_data);

      return lisp_query;
    }
}

DEFUN ("treesit-query-capture",
       Ftreesit_query_capture,
       Streesit_query_capture, 2, 5, 0,
       doc: /* Query NODE with patterns in QUERY.

Return a list of (CAPTURE_NAME . NODE).  CAPTURE_NAME is the name
assigned to the node in PATTERN.  NODE is the captured node.

QUERY is either a string query, a sexp query, or a compiled query.
See Info node `(elisp)Pattern Matching' for how to write a query in
either string or s-expression form.  When using repeatedly, a compiled
query is much faster than a string or sexp one, so it is recommend to
compile your queries if it will be used over and over.

BEG and END, if both non-nil, specifies the range in which the query
is executed.  If NODE-ONLY is non-nil, return a list of nodes.

Besides a node, NODE can also be a parser, then the root node of that
parser is used; NODE can be a language symbol, then the root node of a
parser for that language is used.  If such a parser doesn't exist, it
is created.

Signals treesit-query-error if QUERY is malformed or something else
goes wrong.  You can use `treesit-query-validate' to debug the
query.  */)
  (Lisp_Object node, Lisp_Object query,
   Lisp_Object beg, Lisp_Object end, Lisp_Object node_only)
{
  if (!NILP (beg))
    CHECK_INTEGER (beg);
  if (!NILP (end))
    CHECK_INTEGER (end);

  if (!(TS_COMPILED_QUERY_P (query)
	|| CONSP (query) || STRINGP (query)))
    wrong_type_argument (Qtreesit_query_p, query);

  /* Resolve NODE into an actual node. */
  Lisp_Object lisp_node;
  if (TS_NODEP (node))
    lisp_node = node;
  else if (TS_PARSERP (node))
    lisp_node = Ftreesit_parser_root_node (node);
  else if (SYMBOLP (node))
    {
      Lisp_Object parser
	= Ftreesit_parser_create (node, Fcurrent_buffer (), Qnil);
      lisp_node = Ftreesit_parser_root_node (parser);
    }
  else
    xsignal2 (Qwrong_type_argument,
	      list4 (Qor, Qtreesit_node_p,
		     Qtreesit_parser_p, Qsymbolp),
	      node);

  /* Extract C values from Lisp objects.  */
  TSNode ts_node = XTS_NODE (lisp_node)->node;
  Lisp_Object lisp_parser = XTS_NODE (lisp_node)->parser;
  ptrdiff_t visible_beg =
    XTS_PARSER (XTS_NODE (lisp_node)->parser)->visible_beg;
  const TSLanguage *lang = ts_parser_language
    (XTS_PARSER (lisp_parser)->parser);

  /* Initialize query objects.  At the end of this block, we should
     have a working TSQuery and a TSQueryCursor.  */
  TSQuery *ts_query;
  TSQueryCursor *cursor;
  bool needs_to_free_query_and_cursor;
  if (TS_COMPILED_QUERY_P (query))
    {
      Lisp_Object signal_symbol = Qnil;
      Lisp_Object signal_data = Qnil;
      ts_query = ts_ensure_query_compiled
	(query, &signal_symbol, &signal_data);
      cursor = XTS_COMPILED_QUERY (query)->cursor;
      /* We don't need to free ts_query and cursor because they
	 are stored in a lisp object, which is tracked by gc.  */
      needs_to_free_query_and_cursor = false;
      if (ts_query == NULL)
	{
	  xsignal (signal_symbol, signal_data);
	}
    }
  else
    {
      /* Since query is not TS_COMPILED_QUERY, it can only be a string
	 or a cons.  */
      if (CONSP (query))
	query = Ftreesit_query_expand (query);
      char *query_string = SSDATA (query);
      uint32_t error_offset;
      TSQueryError error_type;
      ts_query = ts_query_new (lang, query_string, strlen (query_string),
			       &error_offset, &error_type);
      if (ts_query == NULL)
	{
	  xsignal (Qtreesit_query_error, ts_compose_query_signal_data
		   (error_offset, error_type));
	}
      cursor = ts_query_cursor_new ();
      needs_to_free_query_and_cursor = true;
    }

  /* WARN: After this point, free ts_query and cursor before every
     signal and return.  */

  /* Set query range. */
  if (!NILP (beg) && !NILP (end))
    {
      EMACS_INT beg_byte = XFIXNUM (beg);
      EMACS_INT end_byte = XFIXNUM (end);
      ts_query_cursor_set_byte_range
	(cursor, (uint32_t) beg_byte - visible_beg,
	 (uint32_t) end_byte - visible_beg);
    }

  /* Execute query.  */
  ts_query_cursor_exec (cursor, ts_query, ts_node);
  TSQueryMatch match;

  /* Go over each match, collect captures and predicates.  Include the
     captures in the RESULT list unconditionally as we get them, then
     test for predicates.  If predicates pass, then all good, if
     predicates don't pass, revert the result back to the result
     before this loop (PREV_RESULT). (Predicates control the entire
     match.) This way we don't need to create a list of captures in
     every for loop and nconc it to RESULT every time.  That is indeed
     the initial implementation in which Yoav found nconc being the
     bottleneck (98.4% of the running time spent on nconc).  */
  Lisp_Object result = Qnil;
  Lisp_Object prev_result = result;
  while (ts_query_cursor_next_match (cursor, &match))
    {
      /* Record the checkpoint that we may roll back to.  */
      prev_result = result;
      /* Get captured nodes.  */
      const TSQueryCapture *captures = match.captures;
      for (int idx=0; idx < match.capture_count; idx++)
	{
	  uint32_t capture_name_len;
	  TSQueryCapture capture = captures[idx];
	  Lisp_Object captured_node =
	    make_ts_node(lisp_parser, capture.node);

	  Lisp_Object cap;
	  if (NILP (node_only))
	    {
	      const char *capture_name = ts_query_capture_name_for_id
		(ts_query, capture.index, &capture_name_len);
	      cap =
		Fcons (intern_c_string_1 (capture_name, capture_name_len),
		       captured_node);
	    }
	  else
	    {
	      cap = captured_node;
	    }
	  result = Fcons (cap, result);
	}
      /* Get predicates.  */
      Lisp_Object predicates =
	ts_predicates_for_pattern (ts_query, match.pattern_index);

      /* captures_lisp = Fnreverse (captures_lisp); */
      struct capture_range captures_range = { result, prev_result };
      if (!ts_eval_predicates (captures_range, predicates))
	{
	  /* Predicates didn't pass, roll back.  */
	  result = prev_result;
	}
    }
  if (needs_to_free_query_and_cursor)
    {
      ts_query_delete (ts_query);
      ts_query_cursor_delete (cursor);
    }
  return Fnreverse (result);
}

/*** Navigation */

/* Return the next/previous named/unnamed sibling of NODE.  FORWARD
   controls the direction and NAMED controls the nameness.
 */
static TSNode
ts_traverse_sibling_helper (TSNode node, bool forward, bool named)
{
  if (forward)
    {
      if (named)
	return ts_node_next_named_sibling (node);
      else
	return ts_node_next_sibling (node);
    }
  else
    {
      if (named)
	return ts_node_prev_named_sibling (node);
      else
	return ts_node_prev_sibling (node);
    }
}

/* Return true if NODE matches PRED.  PRED can be a string or a
   function.  This function doesn't check for PRED's type.  */
static bool
ts_traverse_match_predicate
(TSNode node, Lisp_Object pred, Lisp_Object parser)
{
  if (STRINGP (pred))
    {
      const char *type = ts_node_type (node);
      return (fast_c_string_match_ignore_case
	      (pred, type, strlen (type)) >= 0);
    }
  else
    {
      Lisp_Object lisp_node = make_ts_node (parser, node);
      return !NILP (CALLN (Ffuncall, pred, lisp_node));
    }

}

/* Traverse the parse tree starting from ROOT (but ROOT is not
   matches against PRED).  PRED can be a function (takes a node and
   returns nil/non-nil),or a string (treated as regexp matching the
   node's type, ignores case, must be all single byte characters).  If
   the node satisfies PRED , terminate, set ROOT to that node, and
   return true.  If no node satisfies PRED, return FALSE.  PARSER is
   the parser of ROOT.

   LIMIT is the number of levels we descend in the tree.  If NO_LIMIT
   is true, LIMIT is ignored.  FORWARD controls the direction in which
   we traverse the tree, true means forward, false backward.  If NAMED
   is true, only traverse named nodes, if false, all nodes.  If
   SKIP_ROOT is true, don't match ROOT.  */
static bool
ts_search_dfs
(TSNode *root, Lisp_Object pred, Lisp_Object parser,
 bool named, bool forward, ptrdiff_t limit, bool no_limit,
 bool skip_root)
{
  /* TSTreeCursor doesn't allow us to move backward, so we can't use
     it.  We could use limit == -1 to indicate no_limit == true, but
     separating them is safer.  */
  TSNode node = *root;

  if (!skip_root && ts_traverse_match_predicate (node, pred, parser))
    {
      *root = node;
      return true;
    }

  if (!no_limit && limit <= 0)
    return false;
  else
    {
      int count = named ?
	ts_node_named_child_count( node)
	: ts_node_child_count (node);
      for (int offset=0; offset < count; offset++)
	{
	  uint32_t idx = forward ? offset
	    : count - offset - 1;
	  TSNode child = ts_node_child (node, idx);

	  if (!ts_node_is_null (child)
	      && ts_search_dfs (&child, pred, parser, named,
				forward, limit - 1, no_limit, false))
	    {
	      *root = child;
	      return true;
	    }
	}
      return false;
    }
}

/* Go thought the whole tree linearly depth first, starting from
   START.  PRED, PARSER, NAMED, FORWARD are the same as in
   ts_search_subtre.  If UP_ONLY is true, never go to children, only
   sibling and parents.  If SKIP_START is true, don'tt match
   START.  */
static bool
ts_search_forward
(TSNode *start, Lisp_Object pred, Lisp_Object parser,
 bool named, bool forward, bool up_only, bool skip_start)
{
  TSNode node = *start;

  if (!up_only && ts_search_dfs
      (start, pred, parser, named, forward, 0, true, skip_start))
    return true;

  TSNode next = ts_traverse_sibling_helper (node, forward, named);
  while (ts_node_is_null (next))
    {
      node = ts_node_parent (node);
      if (ts_node_is_null (node))
	return false;

      if (ts_traverse_match_predicate (node, pred, parser))
	{
	  *start = node;
	  return true;
	}
      next = ts_traverse_sibling_helper (node, forward, named);
    }
  if (ts_search_forward
      (&next, pred, parser, named, forward, up_only, false))
    {
      *start = next;
      return true;
    }
  else
    return false;
}

DEFUN ("treesit-search-subtree",
       Ftreesit_search_subtree,
       Streesit_search_subtree, 2, 5, 0,
       doc: /* Traverse the parse tree depth-first.

Traverse the subtree of NODE, and match PREDICATE with each node along
the way.  PREDICATE is a regexp string that matches against each
node's type case-insensitively, or a function that takes a node and
returns nil/non-nil.

By default, only traverse named nodes, if ALL is non-nil, traverse all
nodes.  If BACKWARD is non-nil, traverse backwards.  If LIMIT is
non-nil, we only traverse that number of levels down in the tree.

Return the first matched node, or nil if none matches.  */)
  (Lisp_Object node, Lisp_Object predicate, Lisp_Object all,
   Lisp_Object backward, Lisp_Object limit)
{
  CHECK_TS_NODE (node);
  CHECK_TYPE (STRINGP (predicate) || FUNCTIONP (predicate),
	      list3 (Qor, Qstringp, Qfunctionp), predicate);
  CHECK_SYMBOL (all);
  CHECK_SYMBOL (backward);

  ptrdiff_t the_limit = 0;
  bool no_limit = false;
  if (NILP (limit))
      no_limit = true;
  else
    {
      CHECK_FIXNUM (limit);
      the_limit = XFIXNUM (limit);
    }

  TSNode ts_node = XTS_NODE (node)->node;
  Lisp_Object parser = XTS_NODE (node)->parser;
  if (ts_search_dfs
      (&ts_node, predicate, parser, NILP (all),
       NILP (backward), the_limit, no_limit, false))
    {
      return make_ts_node (parser, ts_node);
    }
  else
    return Qnil;
}

DEFUN ("treesit-search-forward",
       Ftreesit_search_forward,
       Streesit_search_forward, 2, 5, 0,
       doc: /* Search for node in the parse tree.

Start traversing the tree from node START, and match PREDICATE with
each node along the way (except START).  PREDICATE is a regexp string
that matches against each node's type case-insensitively, or a
function that takes a node and returns nil/non-nil.

By default, only search for named nodes, if ALL is non-nil, search for
all nodes.  If BACKWARD is non-nil, search backwards.

Return the first matched node, or nil if none matches.

For a tree like the below where START is marked 1, traverse as
numbered:
                16
                |
       3--------4-----------8
       |        |           |
  o--o-+--1  5--+--6    9---+-----12
  |  |    |        |    |         |
  o  o    2        7  +-+-+    +--+--+
                      |   |    |  |  |
                      10  11   13 14 15

If UP is non-nil, only traverse to siblings and parents.  In that
case, only 1 3 4 8 16 would be traversed.  */)
  (Lisp_Object start, Lisp_Object predicate, Lisp_Object all,
   Lisp_Object backward, Lisp_Object up)
{
  CHECK_TS_NODE (start);
  CHECK_TYPE (STRINGP (predicate) || FUNCTIONP (predicate),
	      list3 (Qor, Qstringp, Qfunctionp), predicate);
  CHECK_SYMBOL (all);
  CHECK_SYMBOL (backward);
  CHECK_SYMBOL (up);

  TSNode ts_start = XTS_NODE (start)->node;
  Lisp_Object parser = XTS_NODE (start)->parser;
  if (ts_search_forward
      (&ts_start, predicate, parser, NILP (all),
       NILP (backward), !NILP (up), true))
    {
      return make_ts_node (parser, ts_start);
    }
  else
    return Qnil;
}

/* Recursively traverse the tree under CURSOR, and append the result
   subtree to PARENT's cdr.  See more in Ftreesit_induce_sparse_tree.
   Note that the top-level children list is reversed, because
   reasons.  */
static void
ts_build_sparse_tree
(TSTreeCursor *cursor, Lisp_Object parent, Lisp_Object pred,
 Lisp_Object process_fn, ptrdiff_t limit,
 bool no_limit, Lisp_Object parser)
{

  TSNode node = ts_tree_cursor_current_node (cursor);
  bool match = ts_traverse_match_predicate (node, pred, parser);
  if (match)
    {
      /* If this node matches pred, add a new node to the parent's
	 children list.  */
      Lisp_Object lisp_node = make_ts_node (parser, node);
      if (!NILP (process_fn))
	{
	  lisp_node = CALLN (Ffuncall, process_fn, lisp_node);
	}
      Lisp_Object this = Fcons (lisp_node, Qnil);
      Fsetcdr (parent, Fcons (this, Fcdr (parent)));
      /* Now for children nodes, this is the new parent.  */
      parent = this;
    }
  /* Go through each child.  */
  if ((no_limit || limit > 0)
      && ts_tree_cursor_goto_first_child (cursor))
    {
      do
	{
	  /* Make sure not to use node after the recursive funcall.
	     Then C compilers should be smart enough not to copy NODE
	     to stack.  */
	  ts_build_sparse_tree
	    (cursor, parent, pred, process_fn,
	     limit - 1, no_limit, parser);
	}
      while (ts_tree_cursor_goto_next_sibling (cursor));
      /* Don't forget to come back to this node.  */
      ts_tree_cursor_goto_parent (cursor);
    }
  /* Before we go, reverse children in the sparse tree.  */
  if (match)
    {
      /* When match == true, "parent" is actually the node we added in
	 this layer (parent = this).  */
      Fsetcdr (parent, Fnreverse (Fcdr (parent)));
    }
}

DEFUN ("treesit-induce-sparse-tree",
       Ftreesit_induce_sparse_tree,
       Streesit_induce_sparse_tree, 2, 4, 0,
       doc: /* Create a sparse tree of ROOT's subtree.

Basically, take the subtree under ROOT, and comb it so only the nodes
that match PREDICATE are left, like picking out grapes on the vine.
PREDICATE is a regexp string that matches against each node's type
case-insensitively.

For a subtree on the left that consist of both numbers and letters, if
PREDICATE is "is letter", the returned tree is the one on the right.

	a                 a              a
	|                 |              |
    +---+---+         +---+---+      +---+---+
    |   |   |         |   |   |      |   |   |
    b   1   2         b   |   |      b   c   d
	|   |     =>      |   |  =>      |
	c   +--+          c   +          e
	|   |  |          |   |
     +--+   d  4       +--+   d
     |  |              |
     e  5              e

If PROCESS-FN is non-nil, instead of returning the matched nodes, pass
each node to PROCESS-FN use the return value instead.  If non-nil,
LIMIT is the number of levels to go down from ROOT.

Each node in the returned tree looks like (NODE . (CHILD ...)).  The
root of this tree might be nil, if ROOT doesn't match PREDICATE.  If
no node matches PRED, return nil.

PREDICATE can also be a function that takes a node and returns
nil/non-nil, but it is slower and more memory consuming than
regexp.  */)
  (Lisp_Object root, Lisp_Object predicate, Lisp_Object process_fn,
   Lisp_Object limit)
{
  CHECK_TS_NODE (root);
  CHECK_TYPE (STRINGP (predicate) || FUNCTIONP (predicate),
	      list3 (Qor, Qstringp, Qfunctionp), predicate);

  if (!NILP (process_fn))
    CHECK_TYPE (FUNCTIONP (process_fn), Qfunctionp, process_fn);
  ptrdiff_t the_limit = 0;
  bool no_limit = false;
  if (NILP (limit))
      no_limit = true;
  else
    {
      CHECK_FIXNUM (limit);
      the_limit = XFIXNUM (limit);
    }

  TSTreeCursor cursor = ts_tree_cursor_new (XTS_NODE (root)->node);
  Lisp_Object parser = XTS_NODE (root)->parser;
  Lisp_Object parent = Fcons (Qnil, Qnil);
  ts_build_sparse_tree
    (&cursor, parent, predicate, process_fn,
     the_limit, no_limit, parser);
  Fsetcdr (parent, Fnreverse (Fcdr (parent)));
  if (NILP (Fcdr (parent)))
    return Qnil;
  else
    return parent;
}

/*** Initialization */

/* Initialize the tree-sitter routines.  */
void
syms_of_treesit (void)
{
  DEFSYM (Qtreesit_parser_p, "treesit-parser-p");
  DEFSYM (Qtreesit_node_p, "treesit-node-p");
  DEFSYM (Qtreesit_compiled_query_p, "treesit-compiled-query-p");
  DEFSYM (Qtreesit_query_p, "treesit-query-p");
  DEFSYM (Qnamed, "named");
  DEFSYM (Qmissing, "missing");
  DEFSYM (Qextra, "extra");
  DEFSYM (Qhas_changes, "has-changes");
  DEFSYM (Qhas_error, "has-error");

  DEFSYM (Qnot_found, "not-found");
  DEFSYM (Qsymbol_error, "symbol-error");
  DEFSYM (Qversion_mismatch, "version-mismatch");

  DEFSYM (Qtreesit_error, "treesit-error");
  DEFSYM (Qtreesit_query_error, "treesit-query-error");
  DEFSYM (Qtreesit_parse_error, "treesit-parse-error");
  DEFSYM (Qtreesit_range_invalid, "treesit-range-invalid");
  DEFSYM (Qtreesit_buffer_too_large,
	  "treesit-buffer-too-large");
  DEFSYM (Qtreesit_load_language_error,
	  "treesit-load-language-error");
  DEFSYM (Qtreesit_node_outdated,
	  "treesit-node-outdated");
  DEFSYM (Quser_emacs_directory,
	  "user-emacs-directory");
  DEFSYM (Qtreesit_parser_deleted, "treesit-parser-deleted");

  DEFSYM (Qor, "or");

  define_error (Qtreesit_error, "Generic tree-sitter error", Qerror);
  define_error (Qtreesit_query_error, "Query pattern is malformed",
		Qtreesit_error);
  /* Should be impossible, no need to document this error.  */
  define_error (Qtreesit_parse_error, "Parse failed",
		Qtreesit_error);
  define_error (Qtreesit_range_invalid,
		"RANGES are invalid, they have to be ordered and not overlapping",
		Qtreesit_error);
  define_error (Qtreesit_buffer_too_large, "Buffer too large (> 4GB)",
		Qtreesit_error);
  define_error (Qtreesit_load_language_error,
		"Cannot load language definition",
		Qtreesit_error);
  define_error (Qtreesit_node_outdated,
		"This node is outdated, please retrieve a new one",
		Qtreesit_error);
  define_error (Qtreesit_parser_deleted,
		"This parser is deleted and cannot be used",
		Qtreesit_error);

  DEFVAR_LISP ("treesit-load-name-override-list",
	       Vtreesit_load_name_override_list,
	       doc:
	       /* An override list for unconventional tree-sitter libraries.

By default, Emacs assumes the dynamic library for LANG is
libtree-sitter-LANG.EXT, where EXT is the OS specific extension for
dynamic libraries.  Emacs also assumes that the name of the C function
the library provides is tree_sitter_LANG.  If that is not the case,
add an entry

    (LANG LIBRARY-BASE-NAME FUNCTION-NAME)

to this list, where LIBRARY-BASE-NAME is the filename of the dynamic
library without extension, FUNCTION-NAME is the function provided by
the library.  */);
  Vtreesit_load_name_override_list = Qnil;

  DEFVAR_LISP ("treesit-extra-load-path",
	       Vtreesit_extra_load_path,
	       doc:
	       /* Extra load paths of tree-sitter language definitions.
When trying to load a tree-sitter language definition,
Emacs looks at directories in this variable,
`user-emacs-directory'/tree-sitter, and system default locations for
dynamic libraries, in that order.  */);
  Vtreesit_extra_load_path = Qnil;

  defsubr (&Streesit_language_available_p);
  defsubr (&Streesit_language_version);

  defsubr (&Streesit_parser_p);
  defsubr (&Streesit_node_p);
  defsubr (&Streesit_compiled_query_p);
  defsubr (&Streesit_query_p);
  defsubr (&Streesit_query_language);

  defsubr (&Streesit_node_parser);

  defsubr (&Streesit_parser_create);
  defsubr (&Streesit_parser_delete);
  defsubr (&Streesit_parser_list);
  defsubr (&Streesit_parser_buffer);
  defsubr (&Streesit_parser_language);

  defsubr (&Streesit_parser_root_node);
  /* defsubr (&Streesit_parse_string); */

  defsubr (&Streesit_parser_set_included_ranges);
  defsubr (&Streesit_parser_included_ranges);

  defsubr (&Streesit_node_type);
  defsubr (&Streesit_node_start);
  defsubr (&Streesit_node_end);
  defsubr (&Streesit_node_string);
  defsubr (&Streesit_node_parent);
  defsubr (&Streesit_node_child);
  defsubr (&Streesit_node_check);
  defsubr (&Streesit_node_field_name_for_child);
  defsubr (&Streesit_node_child_count);
  defsubr (&Streesit_node_child_by_field_name);
  defsubr (&Streesit_node_next_sibling);
  defsubr (&Streesit_node_prev_sibling);
  defsubr (&Streesit_node_first_child_for_pos);
  defsubr (&Streesit_node_descendant_for_range);
  defsubr (&Streesit_node_eq);

  defsubr (&Streesit_pattern_expand);
  defsubr (&Streesit_query_expand);
  defsubr (&Streesit_query_compile);
  defsubr (&Streesit_query_capture);

  defsubr (&Streesit_search_subtree);
  defsubr (&Streesit_search_forward);
  defsubr (&Streesit_induce_sparse_tree);
}