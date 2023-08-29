/* example/tcp_serv.cpp */
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

// Abstract class for handling i/o events on descriptors
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

// Wrapper around event selection cycle (i/e main cycle)
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

// Now, we write the server and sessions, which will both be fd handlers
// plucked into the main cycle
enum {
    max_line_len = 1023,
    qlen_for_listen = 16
};

class ChatServer;

// Fully private, thus accessed only through server 
// (unless used through and FdHandler *)
class ChatSession : FdHandler {
    friend class ChatServer;

    char buffer[max_line_len+1];
    int buf_used;
    bool ignoring;
    char *name;
    ChatServer *master;

    ChatSession(ChatServer *a_master, int fd);
    ~ChatSession();
    void Send(const char *msg);
    virtual void Handle(bool r, bool w);
    void ReadAndIgnore();
    void ReadAndCheck();
    void CheckLines();
    void ProcessLine(const char *line);
};

class ChatServer : public FdHandler {
    EventSelector *selector;
    struct item {
        ChatSession *s;
        item *next;
    };
    item *first;
    // Move out creation and initialization to static method
    ChatServer(EventSelector *sel, int fd);

public:
    ~ChatServer();
    static ChatServer *Create(EventSelector *sel, int port);
    void RemoveSession(ChatSession *s);
    void SendAll(const char *msg, ChatSession *except = 0);

private:
    virtual void Handle(bool r, bool w);
};

// Now, we just use the program as such:
static int port = 7654;

int main()
{
    EventSelector *selector = new EventSelector;
    ChatServer *serv = ChatServer::Create(selector, port);
    if (!serv) {
        perror("server");
        return 1;
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

ChatServer *ChatServer::Create(EventSelector *sel, int port)
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
    return new ChatServer(sel, ls);
}

ChatServer::ChatServer(EventSelector *sel, int fd)
    : FdHandler(fd, true), selector(sel), first(0) 
{
    selector->Add(this);
}

ChatServer::~ChatServer()
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

void ChatServer::Handle(bool r, bool w)
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
    p->s = new ChatSession(this, sd);
    first = p;
    selector->Add(p->s);
}

void ChatServer::RemoveSession(ChatSession *s)
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

void ChatServer::SendAll(const char *msg, ChatSession *except)
{
    item *p;
    for (p = first; p; p = p->next) {
        if (p->s != except)
            p->s->Send(msg);
    }
}

void ChatSession::Send(const char *msg)
{
    write(GetFd(), msg, strlen(msg));
}

ChatSession::ChatSession(ChatServer *a_master, int fd)
    : FdHandler(fd, true), buf_used(0), ignoring(false),
    name(0), master(a_master)
{
    Send("Provide your name, please: ");
}

ChatSession::~ChatSession()
{
    if (name)
        delete[] name;
}

static const char welcome_msg[] = "Welcome to the chat, you are known as ";
static const char entered_msg[] = " has entered the chat";
static const char left_msg[] = " has left the chat";

void ChatSession::Handle(bool r, bool w)
{
    if (!r) // Erroneous situation
        return;

    // If line is too long, ignore it entirely, but don't disconnect
    if (buf_used >= (int)sizeof(buffer)) {
        buf_used = 0;
        ignoring = true;
    }

    if (ignoring)
        ReadAndIgnore();
    else
        ReadAndCheck();
}

void ChatSession::ReadAndIgnore()
{
    int rc = read(GetFd(), buffer, sizeof(buffer));
    if (rc < 1) {
        master->RemoveSession(this);
        return;
    }
    for (int i = 0; i < rc; i++) {
        if (buffer[i] == '\n') {
            int rest = rc-i-1;
            if (rest > 0)
                memmove(buffer, buffer+i+1, rest);
            buf_used = rest;
            ignoring = false;
        }
    }
}

void ChatSession::ReadAndCheck()
{
    int rc = read(GetFd(), buffer, sizeof(buffer));
    if (rc < 1) {
        if (name) {
            int len = strlen(name);
            char *lmsg = new char[len + sizeof(left_msg) + 1];
            sprintf(lmsg, "%s%s\n", name, left_msg);
            master->SendAll(lmsg, this);
            delete[] lmsg;
        }
        master->RemoveSession(this);
        return;
    }
    buf_used += rc;
    CheckLines();
}

void ChatSession::CheckLines()
{
    if (buf_used <= 0)
        return;
    for (int i = 0; i < buf_used; i++) {
        if (buffer[i] == '\n') {
            buffer[i] = 0;
            if (i > 0 && buffer[i-1] == '\r')
                buffer[i-1] = 0;
            ProcessLine(buffer);
            int rest = buf_used-i-1;
            memmove(buffer, buffer+i+1, rest);
            buf_used = rest;
            CheckLines();
            return;
        }
    }
}

void ChatSession::ProcessLine(const char *line)
{
    int len = strlen(line);
    if (!name) {
        name = new char[len+1];
        strcpy(name, line);
        char *wmsg = new char[len + sizeof(welcome_msg) + 1];
        sprintf(wmsg, "%s%s\n", welcome_msg, name);
        Send(wmsg);
        delete[] wmsg;
        char *emsg = new char[len + sizeof(entered_msg) + 1];
        sprintf(emsg, "%s%s\n", name, entered_msg);
        master->SendAll(emsg, this);
        delete[] emsg;
        return;
    }
    int nl = strlen(name);
    char *msg = new char[nl + len + 5];
    sprintf(msg, "<%s> %s\n", name, line);
    master->SendAll(msg, this);
    delete[] msg;
}
