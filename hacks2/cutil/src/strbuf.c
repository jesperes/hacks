/*
 * string.c
 *
 *  Created on: Oct 7, 2008
 *      Author: Jesper
 */

#include "strbuf.h"
#include "string.h"

void sb_init(strbuf_t *s)
{
  s->buf = NULL;
  s->length = 0;
  s->bufsize = 0;
}

size_t sb_length(strbuf_t *s) {
  return s->length;
}

const char *sb_buf(strbuf_t *s) {
  if (s->buf == NULL) {
    return "";
  } else {
    return s->buf;
  }
}

void sb_ensure_length(strbuf_t *str, size_t len)
{
  if (str->bufsize <= len) {
    str->bufsize = len + 1;
    str->buf = realloc(str->buf, str->bufsize);
    str->buf[len] = 0;
  }
}

void sb_set_cstr(strbuf_t *s, const char *cstr)
{
  size_t n = strlen(cstr);
  sb_ensure_length(s, n);
  memcpy(s->buf, cstr, n);
  s->buf[n] = 0;
  s->length = n;
}

void sb_set(strbuf_t *s, strbuf_t *from)
{
  size_t n = sb_length(from);
  sb_ensure_length(s, n);
  memcpy(s->buf, sb_buf(from), n);
  s->buf[n] = 0;
  s->length = n;
}

void sb_clear(strbuf_t *s)
{
  free(s->buf);
  s->buf = NULL;
  s->length = 0;
}

char *sb_detach(strbuf_t *s)
{
  char *buf = s->buf;
  s->buf = NULL;
  s->length = 0;
  s->bufsize = 0;
  return buf;
}
