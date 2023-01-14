/* 4_33/calc.c */
int sys_read(int fd, void *buf, int size);
int sys_write(int fd, const void *buf, int size);

enum { 
    in_bufsize = 128, out_bufsize = 11, 
    rpn_stk_size = 64, sgn_stk_size = 128 
};

static const char input_err[] = "Invalid expression\n";

static char in_buf[in_bufsize];
static char out_buf[out_bufsize];
static int rpn_stk[rpn_stk_size];
static char sgn_stk[sgn_stk_size];

int char_is_digit(char c)
{
    return c >= '0' && c <= '9';
}

int char_is_sign(char c)
{
    return c == '(' || c == ')' || c == '+' || c == '-' ||
           c == '*' || c == '/';
}

int read_input_to_buf()
{
    return sys_read(0, in_buf, in_bufsize);
}

int write_output_from_buf(int len)
{
    return sys_write(1, out_buf, len);
}

int putnl()
{
    return sys_write(1, "\n", 1);
}

int push_int(int num, int *rpn_stk_top)
{
    if (rpn_stk_top - rpn_stk + 1 >= rpn_stk_size)
        return 0;

    rpn_stk_top++;
    *rpn_stk_top = num;
    return 1;
}

int push_sgn(char sgn, char *sgn_stk_top)
{
    if (sgn_stk_top - sgn_stk + 1 >= sgn_stk_size)
        return 0;

    sgn_stk_top++;
    *sgn_stk_top = sgn;
    return 1;
}

int parse_int(const char *str, int strlen, int *out, char *break_chr)
{
    int chars_read = 1;

    *out = 0;
    for (; chars_read <= strlen; str++, chars_read++) {
        if (!char_is_digit(*str))
            break;

        *out *= 10;
        *out += *str - '0';
    }

    *break_chr = *str;
    return chars_read;
}

int stringify_int(int num, char *dest, int destlen)
{
    int status = 0;
    char *lp, *fp;

    lp = dest;
    for (; lp - dest < destlen; lp++) {
        *lp = num%10 + '0';
        num /= 10;
        if (num == 0) {
            status = 1;
            break;
        }
    }

    if (status == 0)
        return status;
    else
        status = lp - dest + 1;

    for (fp = dest; lp - fp > 0; fp++, lp--) {
        char tmp = *fp;
        *fp = *lp;
        *lp = tmp;
    }

    return status;
}

void process_sign(char sgn, int *rpn_stk_top, char *sgn_stk_top)
{
}

int process_int_from_buf(const char *buf, int buflen, 
        int *rpn_stk_top, char *sgn_stk_top)
{
    int chars_read;
    int n;
    char brkchr;

    chars_read = parse_int(buf, buflen, &n, &brkchr);
    if (chars_read > 1)
        push_int(n, rpn_stk_top);

    if (brkchr == '\0')
        return 0;
    else if (char_is_sign(brkchr)) {
        process_sign(brkchr, rpn_stk_top, sgn_stk_top);
        return chars_read;
    } else
        return -1;
}

int main()
{
    /* test */
    int nchars;
    int num1, num2;
    char brchr;

    read_input_to_buf();
    parse_int(in_buf, in_bufsize, &num1, &brchr);

    read_input_to_buf();
    parse_int(in_buf, in_bufsize, &num2, &brchr);

    num1 += num2;

    nchars = stringify_int(num1, out_buf, out_bufsize);
    write_output_from_buf(nchars);
    putnl();

    return 0;
}
