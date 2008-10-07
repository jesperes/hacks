/*
 * runtests.c
 *
 *  Created on: Oct 7, 2008
 *      Author: Jesper
 */

#include <strbuf.h>
#include <unittest.h>

UNITTEST(sb, init)
{
	strbuf_t sb = SB_INIT;
	UNITTEST_ASSERT(sb.length != 0);
	UNITTEST_RETURN;
}

int main()
{
	UNITTEST_RUN(sb, init);
	UNITTEST_FINISH;
	return UNITTEST_STATUS;
}
