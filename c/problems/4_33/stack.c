/* 4_33/stack.c */

enum { stk_cap = 127 };

#define MAKE_STACK_STRUCT(TYPE) \
    typedef struct TYPE ## _stack_tag { \
        TYPE content[stk_cap + 1]; \
        TYPE *top; \
    } TYPE ## _stack

MAKE_STACK_STRUCT(char);
MAKE_STACK_STRUCT(int);

#define MAKE_STACK_IS_FULL(TYPE) \
    int TYPE ## _stack_is_full(TYPE ## _stack *sp) \
    { \
        return sp->top - sp->content >= stk_cap; \
    }

#define MAKE_STACK_IS_EMPTY(TYPE) \
    int TYPE ## _stack_is_empty(TYPE ## _stack *sp) \
    { \
        return sp->top - sp->content <= 0; \
    }

#define MAKE_STACK_PUSH(TYPE) \
    int TYPE ## _stack_push(TYPE elem, TYPE ## _stack *sp) \
    { \
        if (TYPE ## _stack_is_full(sp)) \
            return 0; \
        *(sp->top) = elem; \
        (sp->top)++; \
        return 1; \
    }

#define MAKE_STACK_POP(TYPE) \
    int TYPE ## _stack_pop(TYPE ## _stack *sp, TYPE *out) \
    { \
        if (TYPE ## _stack_is_empty(sp)) \
            return 0; \
        (sp->top)--; \
        *out = *(sp->top); \
        return 1; \
    }

MAKE_STACK_IS_FULL(char);
MAKE_STACK_IS_EMPTY(char);
MAKE_STACK_PUSH(char);
MAKE_STACK_POP(char);

MAKE_STACK_IS_FULL(int);
MAKE_STACK_IS_EMPTY(int);
MAKE_STACK_PUSH(int);
MAKE_STACK_POP(int);
