/*
 * runtests.c
 *
 *  Created on: Oct 7, 2008
 *      Author: Jesper
 */

#include <strbuf.h>
#include <unittest.h>
#include <string.h>

UNITTEST(sb, init)
{
	strbuf_t sb = SB_INIT;
	UNITTEST_ASSERT(sb.length == 0);
	UNITTEST_ASSERT(sb_length(&sb) == 0);
	UNITTEST_RETURN;
}

UNITTEST(sb, clear)
{

	UNITTEST_RETURN;
}

UNITTEST(sb, ensure_length)
{
	strbuf_t sb = SB_INIT;

	sb_ensure_length(&sb, 256);
	UNITTEST_ASSERT(sb_length(&sb) == 256);

	sb_ensure_length(&sb, 128);
	UNITTEST_ASSERT(sb_length(&sb) == 256);

	sb_ensure_length(&sb, 4096);
	UNITTEST_ASSERT(sb_length(&sb) == 4096);

	sb_clear(&sb);
	UNITTEST_ASSERT(sb_length(&sb) == 0);
	UNITTEST_RETURN;
}

UNITTEST(sb, assign)
{
	strbuf_t sb = SB_INIT;
	const char *cstr = "foobar";

	sb_assign_cstr(&sb, cstr);
	UNITTEST_ASSERT(sb_length(&sb) == strlen(cstr));
	UNITTEST_ASSERT(strcmp(sb_buf(&sb), cstr) == 0);

	UNITTEST_RETURN;
}

int main()
{
	UNITTEST_RUN(sb, init);
	UNITTEST_RUN(sb, ensure_length);
	UNITTEST_RUN(sb, assign);

	UNITTEST_FINISH;
	return UNITTEST_STATUS;
}
