#ifndef NODE_H
#define NODE_H

#include <apr_time.h>
#include <apr_pools.h>

/* Represents a node in the dependency graph */
struct node_t
{
  struct node_t *next;

  char *fsname;
  apr_time_t timestamp;
  apr_pool_t *pool;

  struct node_t *children;
};

typedef struct node_t node_t;

node_t *node_create(apr_pool_t *pool);

// Bind node to a filesystem name
void node_bind(node_t *node, const char *fsname);

// Add a child node to this node
void node_add_child(node_t *node, node_t *child);

// Print a representation of node to stdou;
void node_inspect(node_t *node, int level);


#endif // NODE_H
		    
