/* basics/file.cpp */
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

class File {
    int fd;

public:
    File() { fd = -1; };
    
    bool OpenRO(const char *name) {
        fd = open(name, O_RDONLY);
        return fd != -1;
    }

    // Other methods

    // A destructor: this is called when the object ceases to exist
    // (goes out of scope), and is used to free resources in RAII
    ~File() { if (fd != -1) close(fd); }
};

int main()
{
    char namebuf[128];
    int len = read(0, namebuf, sizeof(namebuf));
    namebuf[len-1] = '\0';

    File f;
    bool res = f.OpenRO(namebuf);
    printf(res ? "Opened!\n" : "Failed to open\n");

    // Here the destructor shall be called
    return 0;
}
