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
	s->buf = "";
	s->length = 0;
}

size_t sb_length(strbuf_t *s) {
	return s->length;
}

const char *sb_buf(strbuf_t *s) {
	return s->buf;
}

void sb_assign_cstr(strbuf_t *s, const char *cstr)
{
	size_t n = strlen(cstr);
	sb_ensure_length(s, n);
	memcpy(s->buf, cstr, n);

	// the allocated buffer will be n+1 bytes long
	s->buf[n] = 0;	// there may be trailing junk in buf
	s->length = n;
}

void sb_clear(strbuf_t *s) {
	free(s->buf);
	s->buf = "";
	s->length = 0;
}

void sb_ensure_length(strbuf_t *str, size_t len)
{
	if (str->length < len) {
		str->buf = realloc(str->buf, len + 1);
		str->buf[len-1] = 0;
		str->length = len;
	}
}
