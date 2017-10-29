#include <stdio.h>
#include <apr.h>

#include "env.h"
#include "node.h"

int 
main(int argc, 
     const char * const *argv, 
     const char * const *env_p)
{
  env_t *env;
  node_t *node, *c;

  apr_app_initialize(&argc, &argv, &env_p);

  env = env_create();
  node = node_create(env->pool);

  node_bind(node, "/usr/bin/ls");

  node_add_child(node, node_create(env->pool));
  node_add_child(node, node_create(env->pool));
  node_add_child(node, node_create(env->pool));
  node_add_child(node, node_create(env->pool));

  c = node_create(env->pool);
  node_add_child(c, node_create(env->pool));
  node_add_child(c, node_create(env->pool));
  node_add_child(c, node_create(env->pool));

  node_add_child(node, c);

  node_inspect(node, 0);
  
  env_destroy(env);
  apr_terminate();

  return 0;
}
