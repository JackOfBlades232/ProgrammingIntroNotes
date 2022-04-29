program QOL; { qol.pas }
type
	LongItemPtr = ^LongItem;
	LongItem = record
		data: longint;
		next: LongItemPtr;
	end;
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

function QOLIsEmpty(var queue: QueueOfLongints): boolean;
begin
	QOLIsEmpty := queue.first = nil
end;

var
	q1, q2: QueueOfLongints;
	n: longint;
begin { double number problem through queue interface }
	QOLInit(q1);
	QOLInit(q2);
	while not SeekEof do
	begin
		read(n);
		QOLPut(q1, n);
		QOLPut(q2, n)
	end;
	while not QOLIsEmpty(q1) do
	begin
		QOLGet(q1, n);
		writeln(n)
	end;
	while not QOLIsEmpty(q2) do
	begin
		QOLGet(q2, n);
		writeln(n)
	end
end.
