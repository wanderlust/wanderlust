/* Dynamic loading module to compute MD5 for Emacs 20 with DL support.

   Copyright (C) 1999 Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING.  If not, write to
   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

*/

/*
  How to compile: (OpenSSL is required)

  gcc -shared -nostdlib -fPIC -I${EMACS}/src -o md5.so md5-dl.c -lcrypto

*/

#include "config.h"
#include "lisp.h"

/* for 20.2 (not tested)
#ifndef STRING_BYTES
#define STRING_BYTES(STR) ((STR)->size)
#define make_unibyte_string make_string
#endif
*/

#include <openssl/md5.h>

static unsigned char to_hex[] = {'0', '1', '2', '3', '4', '5', '6', '7',
                                 '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

DEFUN ("md5-string", Fmd5_string, Smd5_string, 1, 1, 0,
  "Return the MD5 of the STRING.")
  (string)
     Lisp_Object string;
{
  unsigned char *md;
  unsigned char md_hex[MD5_DIGEST_LENGTH*2];
  int i, j;

  CHECK_STRING (string, 0);

  md = MD5 (XSTRING (string)->data, STRING_BYTES (XSTRING (string)),
            (unsigned char *) 0);

  for (i = j = 0; i < MD5_DIGEST_LENGTH; i++, j++)
    {
      md_hex[j]   = to_hex[(unsigned int)(md[i] / 16)];
      md_hex[++j] = to_hex[(unsigned int)(md[i] & 15)];
    }

  return make_unibyte_string (md_hex, sizeof(md_hex));
}

/*
 * setting
 */
static struct Lisp_Subr *s_md5_string;

void
emacs_md5_init ()
{
  s_md5_string = (struct Lisp_Subr *) xmalloc (sizeof (struct Lisp_Subr));
  bcopy (&Smd5_string, s_md5_string, sizeof (struct Lisp_Subr));
  defsubr (s_md5_string);
}

void
emacs_md5_fini ()
{
  undefsubr (s_md5_string);
  free (s_md5_string);
}
