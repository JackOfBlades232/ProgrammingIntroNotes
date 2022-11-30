/* all string literals are stored as const char *, so in order to be able to
 * pass literals, need const modifier. Literals usually lie in .text.
 * Fun fact: immutable mem is shared by all instances of a prog, so the 
 * literals are stored only once. */
int string_length(const char *str)
{
    /* in C strings are 0-terminating, (in Pascal, they had a length byte, so
     * were limited by len 255, but you could take length fast */
    const char *p;
    for (p = str; *p; p++)
        {}
    return p - str;
}

void string_copy(char *dest, const char *src)
{
    for (; *src; dest++, src++)
        *dest = *src;
    *dest = 0;
}

int main()
{
    const char *str = "Hello";
    /* string literals can init char/signed char/uns char arrays, these won't
     * be in immutable mem */
    char str1[] = "world";
    /* and in case of pointers, it will be immutable, as pointing to literal.
     * So you won't be able to change elems by index, and in str1 you can */
    char *ptr = "Hello";

    char a, b;

    /* seeing as literals are const char *, these are legal, though stupid */
    a = *"Abra";
    b = "Abra"[2];

    printf("%d\n", string_length(str));

    return 0;
}
