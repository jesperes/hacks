/*
 * string.h
 *
 *  Created on: Oct 7, 2008
 *      Author: Jesper
 */

#ifndef STRBUF_H_
#define STRBUF_H_

#include <stdarg.h>
#include <stdlib.h>

typedef struct {
	size_t length;
	char *buf;
} strbuf_t;

#define SB_INIT {0}

void sb_init(strbuf_t *str);
void sb_clear(strbuf_t *str);
void sb_assign(strbuf_t *str, strbuf_t *app);
void sb_assign_cstr(strbuf_t *str, const char *cstr);

/* String length, not counting the terminating NUL */
size_t sb_length(strbuf_t *str);

/* Return the string as a C-str */
const char *sb_buf(strbuf_t *str);

/* Ensure that str has at least len bytes in its buffer */
void sb_ensure_length(strbuf_t *str, size_t len);

void sb_append(strbuf_t *str, strbuf_t *app);
void sb_append_cstr(strbuf_t *str, const char *cstr);
void sb_append_char(strbuf_t *str, char c);
void sb_add_format(strbuf_t *str, const char *fmt, ...);
void sb_add_vformat(strbuf_t *str, const char *fmt, va_list list);


char *sb_detach(strbuf_t *str);


#endif /* STRBUF_H_ */
