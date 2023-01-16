/* 5_33/calc.c */
#include "stack.c"
#include "charfunc.c"
#include "intstr.c"
#include "io.c"

enum { in_bufsize = 128, out_bufsize = 11 };

static const char input_err[] = "Invalid expression\n";

static char in_buf[in_bufsize];
static char out_buf[out_bufsize];

int apply_arifm(int a, int b, char sgn)
{
    switch (sgn) {
        case '+':
            return a + b;
        case '-':
            return a - b;
        case '*':
            return a * b;
        case '/':
            return a / b;
        default:
            return a;
    }
}

int apply_rpn_step(char sgn, int_stack *rpn_stk)
{
    int status;
    int a, b, res;

    status = char_is_arifm_sign(sgn) &&
        int_stack_pop(rpn_stk, &b) && int_stack_pop(rpn_stk, &a);
    if (!status)
        return 0;

    if (sgn == '/' && b == 0)
        return 0;

    res = apply_arifm(a, b, sgn);
    return int_stack_push(res, rpn_stk);
}

int process_stack(int_stack *rpn_stk, char_stack *sgn_stk)
{
    int status = 1;
    char sgn;

    while (status) {
        status = char_stack_pop(sgn_stk, &sgn);
        if (!status)
            break;

        if (sgn == '(')
            return 1;
        else
            status = apply_rpn_step(sgn, rpn_stk);
    }

    return 0;
} 

int priority_less(char sgn_a, char sgn_b)
{
    return sgn_a == '(' ||
        ((sgn_a == '+' || sgn_a == '-') && (sgn_b == '*' || sgn_b == '/'));
}

int process_arifm_sign(char sgn, int_stack *rpn_stk, char_stack *sgn_stk)
{
    int status;
    char top_sgn;
    
    status = char_stack_pop(sgn_stk, &top_sgn);
    if (!status)
        return 0;

    if (priority_less(top_sgn, sgn)) {
        return char_stack_push(top_sgn, sgn_stk) &&
            char_stack_push(sgn, sgn_stk);
    } else {
        status = apply_rpn_step(top_sgn, rpn_stk);
        
        return status == 0 ?
            status :
            process_arifm_sign(sgn, rpn_stk, sgn_stk);
    }
} 

int process_sign(char sgn, int_stack *rpn_stk, char_stack *sgn_stk)
{
    switch (sgn) {
        case '(':
            return char_stack_push(sgn, sgn_stk);
        case ')':
            return process_stack(rpn_stk, sgn_stk);
        default:
            return process_arifm_sign(sgn, rpn_stk, sgn_stk);
    }
}

int process_int_from_buf(char **buf, int *remaining_bufsize, 
        int_stack *rpn_stk, char_stack *sgn_stk)
{
    int chars_read;
    int n;
    char brkchr;

    if (*remaining_bufsize <= 0)
        return 0;

    chars_read = parse_int(*buf, *remaining_bufsize, &n, &brkchr);
    *buf += chars_read;
    *remaining_bufsize -= chars_read;

    if (chars_read > 1)
        int_stack_push(n, rpn_stk);

    if (char_is_stringbreak(brkchr))
        return process_sign(')', rpn_stk, sgn_stk);
    else if (char_is_sign(brkchr))
        return process_sign(brkchr, rpn_stk, sgn_stk);
    else
        return 0;
}

int main()
{
    int chars_written, status;
    int res;

    int_stack rpn_stk;
    char_stack sgn_stk;

    int in_left;
    char *in_bufp;

    in_bufp = in_buf;
    in_left = read_input_to_buf(in_buf, in_bufsize);
    in_left = remove_spaces(in_buf, in_left);

    rpn_stk.top = rpn_stk.content;
    sgn_stk.top = sgn_stk.content;

    char_stack_push('(', &sgn_stk);

    status = 1;
    do {
        status = process_int_from_buf(&in_bufp, &in_left, &rpn_stk, &sgn_stk);
    } while (status && in_left > 0);

    status = status && char_stack_is_empty(&sgn_stk);

    if (!status) {
        werr(input_err);
        return 1;
    }

    status = int_stack_pop(&rpn_stk, &res) && int_stack_is_empty(&rpn_stk);

    if (status) {
        chars_written = stringify_int(res, out_buf, out_bufsize);
        write_output_from_buf(out_buf, chars_written);
        writec('\n');
    } else {
        werr(input_err);
        return 1;
    }


    return 0;
}
