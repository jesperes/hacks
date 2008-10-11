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
	size_t length;			// length of string
	char *buf;
	size_t bufsize;			// size of allocated buffer
} strbuf_t;

#define SB_INIT {0}

void sb_init(strbuf_t *str);
void sb_clear(strbuf_t *str);
void sb_set(strbuf_t *str, strbuf_t *app);
void sb_set_cstr(strbuf_t *str, const char *cstr);

/* String length, not counting the terminating NUL */
size_t sb_length(strbuf_t *str);

/* Return the string as a C-str */
const char *sb_get(strbuf_t *str);

/* Ensure that the buffer size is at least len bytes */
void sb_ensure_length(strbuf_t *str, size_t len);

char *sb_detach(strbuf_t *str);


#endif /* STRBUF_H_ */
