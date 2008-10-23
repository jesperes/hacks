/*
 * unittest.h
 *
 *  Created on: Oct 7, 2008
 *      Author: Jesper
 */

#ifndef UNITTEST_H_
#define UNITTEST_H_

#include <stdio.h>

/*
 * Unit test macros
 */

static int tests_ok = 0;
static int tests_fail = 0;
static int tests_run = 0;
static int num_assertions = 0;

#define UNITTEST(suite, test) \
	int __unittest__ ## suite ## __ ## test (void)

#define UNITTEST_RETURN return 1

#define UNITTEST_RUN(suite, test) 							\
	do { 													\
		printf("%10s:%-20s: ", #suite, #test); 		\
		fflush(stdout); 									\
		tests_run++;										\
		if (__unittest__ ## suite ## __ ## test ()) { 	\
			printf("OK\n"); 								\
			tests_ok++; 									\
		} else { 											\
			printf("FAILED\n"); 							\
			tests_fail++; 									\
		} 													\
	} while(0)


#define UNITTEST_FINISH 									\
	do { 													\
		printf("Total tests: %d\n", tests_run);				\
		printf("Failed tests: %d\n", tests_fail);			\
		printf("Total assertions: %d\n", num_assertions);	\
	} while (0)

#define UNITTEST_STATUS tests_fail

#define UNITTEST_ASSERT(expr) 								\
	do {													\
		num_assertions++; 									\
		if (!(expr)) { 										\
			printf("\nTest failed (%s:%d): %s is false\n",	\
					__FILE__, __LINE__, # expr);			\
					return 0;								\
		}													\
	} while (0)


#endif /* UNITTEST_H_ */
