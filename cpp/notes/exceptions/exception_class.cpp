/* exceptions/throw_catch.cpp */
#include <stdio.h>
#include <string.h>
#include <errno.h>

// If we want to pass some specific data down to the handler, and/or identify
// our exception somehow, it is wise to use an exception class/struct
// It is not special, but it has to have a copy constructor and a destructor,
// cause it will have to be moving from one stack frame to another, if it is
// local (and why would it not be?)
class FileException {
    int err_code;
    char *filename;
    char *comment;

public:
    FileException(const char *fn, const char *cmt);
    FileException(const FileException &other);
    ~FileException();

    const char *GetName() const { return filename; }
    const char *GetComment() const { return comment; }
    int GetErrno() const { return err_code; }

private:
    static char *strdup(const char *str);
};

FileException::FileException(const char *fn, const char *cmt)
{
    err_code = errno;
    filename = strdup(fn);
    comment = strdup(cmt);
}

FileException::FileException(const FileException &other)
{
    err_code = other.err_code;
    filename = strdup(other.filename);
    comment = strdup(other.comment);
}

FileException::~FileException()
{
    delete[] filename;
    delete[] comment;
}

char *FileException::strdup(const char *str)
{
    char *res = new char[strlen(str)+1];
    strcpy(res, str);
    return res;
}

unsigned int line_count_in_file(const char *filename)
{
    FILE *f = fopen(filename, "r");
    if (!f)
        throw FileException(filename, "couldn't open for line counting");
    int n = 0;
    int c = 0;
    while ((c = fgetc(f)) != EOF) {
        if (c == '\n')
            n++;
    }
    fclose(f);
    return n;
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "Args: <filename>\n");
        return 1;
    }
    try {
        printf("Lines in file: %d\n", line_count_in_file(argv[1]));
    } 
    // This thing can catch FileException (const, &), and also every child
    // of FileException, which is the last case of allowed casting in catch.
    // This allows us to introduce a hierarchy of exceptions
    // (this works only for refs cause pointer casting)
    catch (const FileException &ex) { 
        // Here, both possible casts are used: FE -> FE & -> const FE &
        fprintf(stderr, "File exception: %s (%s): %s\n", 
                ex.GetName(), ex.GetComment(), strerror(ex.GetErrno()));
        return 2;
    } 

    return 0;
}
