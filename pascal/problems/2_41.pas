program Problem2_41; { 2_41.pas }
type
    itemptr = ^item;
    item = record
        data: longint;
        count: integer;
        next: itemptr;
    end;

procedure InitList(var first: itemptr);
begin
    first := nil
end;

procedure AddItem(var first: itemptr; value: longint);
var
    tmp1, tmp2: itemptr;
begin
    if first = nil then
    begin
        new(first);
        first^.data := value;
        first^.count := 1;
        first^.next := nil;
        exit
    end;
    if first^.data = value then
    begin
        first^.count := first^.count + 1;
        exit
    end;
    tmp1 := first; 
    while (tmp1 <> nil) and (tmp1^.data <> value) do
    begin
        tmp2 := tmp1;
        tmp1 := tmp1^.next
    end;
    if tmp1 <> nil then
        tmp1^.count := tmp1^.count + 1
    else
    begin
        new(tmp1);
        tmp1^.data := value;
        tmp1^.count := 1;
        tmp1^.next := nil;
        tmp2^.next := tmp1
    end
end;

var
    first, tmp: itemptr;
    value: longint;
    max: integer;
begin
    InitList(first);
    while not SeekEof do
    begin
        read(value);
        AddItem(first, value)
    end;
    max := 0;
    tmp := first;
    while tmp <> nil do
    begin
        if tmp^.count > max then
            max := tmp^.count;
        tmp := tmp^.next
    end;
    while first <> nil do
    begin
        if first^.count = max then
            writeln(first^.data);
        first := first^.next
    end
end.
