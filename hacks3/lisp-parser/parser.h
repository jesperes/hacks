/* -*- vt-c-mode -*- */

extern int yyerror(const char *s);
extern int yylex(void);

typedef enum {
        Lisp_Type_Integer,
        Lisp_Type_Atom,
        Lisp_Type_String,
        Lisp_Type_Cons,
        Lisp_Type_Nil,
} lisp_type_t;

struct lisp_object {
        lisp_type_t type;
        union {
                int integer;
                char *string;
                struct {
                        struct lisp_object *car;
                        struct lisp_object *cdr;
                } cons;
        } u;
};

typedef struct lisp_object lisp_object_t;

extern void print_lisp_object(lisp_object_t *object);

