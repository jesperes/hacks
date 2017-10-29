#include <stdlib.h>
#include <assert.h>

#include "env.h"

env_t *
env_create()
{
  env_t *env = malloc(sizeof(env_t));

  apr_status_t status = apr_pool_create(&env->pool, NULL);
  assert(status == APR_SUCCESS);
  return env;
}

void
env_destroy(env_t *env)
{
  apr_pool_destroy(env->pool);
  free(env);
}

