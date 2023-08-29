/* 10_18/6_12_remake.cpp */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>

class FdHandler {
    int fd;
    bool owns_fd; // If true, fd is closed on destruction

public:
    FdHandler(int a_fd, bool owns = true)
        : fd(a_fd), owns_fd(owns) {}
    virtual ~FdHandler() 
        { if (owns_fd) close(fd); }

    virtual void Handle(bool r, bool w) = 0;
    int GetFd() const { return fd; }
    virtual bool WantsToRead() const { return true; }
    virtual bool WantsToWrite() const { return false; }
};

class EventSelector {
    FdHandler **fd_array;
    int fd_array_len;
    int max_fd;
    bool quit_flag;

public:
    EventSelector() : fd_array(0), quit_flag(false) {}
    ~EventSelector() { if (fd_array) delete[] fd_array; }

    void Add(FdHandler *h);
    bool Remove(FdHandler *h);
    void BreakLoop() { quit_flag = true; }
    void Run();
};

enum {
    max_line_len = 511,
    qlen_for_listen = 16
};

class Server;

class Session : FdHandler {
    friend class Server;

    char buffer[max_line_len+1];
    Server *master;

    Session(Server *a_master, int a_fd)
        : FdHandler(a_fd, true), master(a_master) {}
    ~Session() {}

    void Send(const char *msg);
    virtual void Handle(bool r, bool w);
};

class Server : public FdHandler {
    EventSelector *selector;
    struct item {
        Session *s;
        item *next;
    };
    item *first;
    // Move out creation and initialization to static method
    Server(EventSelector *sel, int fd);

public:
    ~Server();
    static Server *Create(EventSelector *sel, int port);
    void RemoveSession(Session *s);

private:
    virtual void Handle(bool r, bool w);
};

int main(int argc, char **argv)
{
    char *endptr;
    long port;

    if (argc != 2) {
        fprintf(stderr, "Args: <port>\n");
        return 1;
    }

    port = strtol(argv[1], &endptr, 10);
    if (!*argv[1] || *endptr) {
        fprintf(stderr, "Invalid port number\n");
        return 1;
    }

    EventSelector *selector = new EventSelector;
    Server *serv = Server::Create(selector, port);
    if (!serv) {
        perror("server");
        return 2;
    }
    selector->Run();
    return 0;
}

void EventSelector::Add(FdHandler *h)
{
    int i;
    int fd = h->GetFd();

    if (!fd_array) {
        fd_array_len = fd > 15 ? fd+1 : 16;
        fd_array = new FdHandler *[fd_array_len];
        for (i = 0; i < fd_array_len; i++)
            fd_array[i] = 0;
        max_fd = -1;
    } else if (fd_array_len <= fd) {
        FdHandler **tmp = new FdHandler *[fd+1];
        for (i = 0; i < fd_array_len; i++)
            tmp[i] = i < fd_array_len ? fd_array[i] : 0;
        fd_array_len = fd + 1;
        delete[] fd_array;
        fd_array = tmp;
    }

    if (fd > max_fd)
        max_fd = fd;
    fd_array[fd] = h;
}

bool EventSelector::Remove(FdHandler *h)
{
    int fd = h->GetFd();
    if (fd >= fd_array_len || fd_array[fd] != h)
        return false;
    fd_array[fd] = 0;
    if (fd == max_fd) {
        while (max_fd >= 0 && !fd_array[max_fd])
            max_fd--;
    }
    return true;
}

void EventSelector::Run()
{
    quit_flag = false;

    do {
        int i;
        fd_set rds, wrs;
        FD_ZERO(&rds);
        FD_ZERO(&wrs);

        for (i = 0; i <= max_fd; i++) {
            if (fd_array[i]) {
                if (fd_array[i]->WantsToRead())
                    FD_SET(i, &rds);
                if (fd_array[i]->WantsToWrite())
                    FD_SET(i, &wrs);
            }
        }

        int res = select(max_fd+1, &rds, &wrs, 0, 0);
        if (res < 0) {
            if (errno == EINTR)
                continue;
            else
                break;
        }

        if (res > 0) {
            for (i = 0; i <= max_fd; i++) {
                if (!fd_array[i])
                    continue;
                bool r = FD_ISSET(i, &rds);
                bool w = FD_ISSET(i, &wrs);
                if (r || w)
                    fd_array[i]->Handle(r, w);
            }
        }
    } while (!quit_flag);
}

Server *Server::Create(EventSelector *sel, int port)
{
    int ls, opt, res;
    struct sockaddr_in addr;
    ls = socket(AF_INET, SOCK_STREAM, 0);
    if (ls == -1)
        return 0;
    opt = 1;
    setsockopt(ls, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(port);
    res = bind(ls, (struct sockaddr *) &addr, sizeof(addr));
    if (res == -1)
        return 0;
    res = listen(ls, qlen_for_listen);
    if (res == -1)
        return 0;
    return new Server(sel, ls);
}

Server::Server(EventSelector *sel, int fd)
    : FdHandler(fd, true), selector(sel), first(0) 
{
    selector->Add(this);
}

Server::~Server()
{
    while (first) {
        item *tmp = first;
        first = first->next;
        selector->Remove(tmp->s);
        delete tmp->s;
        delete tmp;
    }
    selector->Remove(this);
}

void Server::Handle(bool r, bool w)
{
    if (!r) // Erroneous situation
        return;

    int sd;
    struct sockaddr_in addr;
    socklen_t len = sizeof(addr);
    sd = accept(GetFd(), (struct sockaddr *) &addr, &len);
    if (sd == -1)
        return;

    item *p = new item;
    p->next = first;
    p->s = new Session(this, sd);
    first = p;
    selector->Add(p->s);
}

void Server::RemoveSession(Session *s)
{
    selector->Remove(s);
    item **p;
    for (p = &first; *p; p = &((*p)->next)) {
        if ((*p)->s == s) {
            item *tmp = *p;
            *p = tmp->next;
            delete tmp->s;
            delete tmp;
            return;
        }
    }
}

void Session::Send(const char *msg)
{
    write(GetFd(), msg, strlen(msg));
}

void Session::Handle(bool r, bool w)
{
    if (!r) // Erroneous situation
        return;

    int rc = read(GetFd(), buffer, sizeof(buffer));
    if (rc < 1) {
        master->RemoveSession(this);
        return;
    }

    for (int i = 0; i < rc; i++) {
        if (buffer[i] == '\n')
            Send("Ok\n");
    }
}
