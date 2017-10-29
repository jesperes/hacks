#include <stdio.h>

#include <apr_tables.h>
#include <apr_strings.h>

#include "node.h"

node_t *
node_create(apr_pool_t *pool)
{
  node_t *node = apr_palloc(pool, sizeof(node_t));

  node->next = NULL;
  node->pool = pool;
  node->fsname = "?";
  node->timestamp = apr_time_now();
  node->children = NULL;

  return node;
}

void 
node_bind(node_t *node, const char *fsname)
{
  node->fsname = apr_pstrdup(node->pool, fsname);
}

void
node_add_child(node_t *node, node_t *child)
{
  node_t *n;

  if (node->children == NULL) {
    node->children = child;
    return;
  }

  for (n = node->children; n != NULL; n = n->next) {
    if (n->next == NULL) {
      n->next = child;
      child->next = NULL;
      break;
    }
  }
}

void
node_inspect(node_t *node, int level)
{
  int i;
  node_t *n;

  for (i = 0; i < level*2; i++)
    putc(' ', stdout);

  printf("node{fsname = \"%s\", timestamp = %lld}\n",
	 node->fsname, node->timestamp);
  
  for (n = node->children; n != NULL; n = n->next) {
    node_inspect(n, level + 1);
  }
}
