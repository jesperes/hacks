/* -*- vt-c-mode -*- */

#include <stdio.h>
#include "parser.h"


static lisp_object_t *
car(lisp_object_t *object)
{
        return object->u.cons.car;
}

static lisp_object_t *
cdr(lisp_object_t *object)
{
        return object->u.cons.cdr;
}

static int
list_p(lisp_object_t *object)
{
        return object->type == Lisp_Type_Cons;
}

static int
nil_p(lisp_object_t *object)
{
        return object == NULL || object->type == Lisp_Type_Nil;
}

static void print_lisp_object0(lisp_object_t *object);

static void
print_lisp_list(lisp_object_t *object)
{
        char *space = "";
        if (!list_p(object))
                return;

        fputc('(', stdout);
        for (; !nil_p(object); object = cdr(object)) {
                fputs(space, stdout);
                space = " ";
                print_lisp_object0(car(object));
        }
        fputc(')', stdout);
}

static void
print_lisp_object0(lisp_object_t *object)
{
        switch (object->type) {
        case Lisp_Type_Integer:
                printf("%d", object->u.integer);
                break;
        case Lisp_Type_String:
                printf("\"%s\"", object->u.string);
                break;
        case Lisp_Type_Atom:
                printf("%s", object->u.string);
                break;
        case Lisp_Type_Cons:
                print_lisp_list(object);
                break;
        case Lisp_Type_Nil:
                printf("nil");
        }
}

void
print_lisp_object(lisp_object_t *object)
{
        print_lisp_object0(object);
        printf("\n");
}
