program QOLDemo;
type
    LongItemPtr = ^LongItem;
    LongItem = record
        data: longint;
        next: LongItemPtr;
    end;
type
    QueueOfLongints = record
        first, last: LongItemPtr;
    end;

procedure QOLInit(var queue: QueueOfLongints);
begin
    queue.first := nil;
    queue.last := nil
end;

procedure QOLPut(var queue: QueueOfLongints; n: longint);
begin
    if queue.first = nil then
    begin
        new(queue.first);
        queue.last := queue.first
    end
    else
    begin
        new(queue.last^.next);
        queue.last := queue.last^.next
    end;
    queue.last^.data := n;
    queue.last^.next := nil
end;

procedure QOLGet(var queue: QueueOfLongints; var n: longint);
var
    tmp: LongItemPtr;
begin
    n := queue.first^.data;
    tmp := queue.first;
    queue.first := queue.first^.next;
    if queue.first = nil then
        queue.last := nil;
    dispose(tmp)
end;

function QOLIsEmpty(var queue: QueueOfLongints) : boolean;
begin
    QOLIsEmpty := queue.first = nil
end;

var
    s: QueueOfLongints;
    n: longint;
begin
    QOLInit(s);
    while not eof do
    begin
        readln(n);
        QOLPut(s, n)
    end;
    while not QOLIsEmpty(s) do
    begin
        QOLGet(s, n);
        writeln(n)
    end
end.
