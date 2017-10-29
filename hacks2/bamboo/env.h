#ifndef ENV_H
#define ENV_H 1

#include <apr_pools.h>

typedef struct
{
  apr_pool_t *pool;
  
} env_t;

env_t *env_create();
void env_destroy();

#endif // ENV_H
