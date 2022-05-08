program Problem2_44; { 2_44.pas }
type
    itemptr = ^item;
    item = record
        c: char;
        next: itemptr;
        count: integer;
    end;

function IsNumber(c: char): boolean;
begin
    IsNumber := (c >= '0') and (c <= '9')
end;

procedure AddNumber(var first, last: itemptr; IsFirst: boolean; c: char);
var
    tmp: itemptr;
begin
    new(tmp);
    tmp^.c := c;
    tmp^.next := nil;
    tmp^.count := 0;
    if IsFirst then
        first := tmp;
    if last <> nil then
        last^.next := tmp;
    last := tmp;
    first^.count := first^.count + 1
end;

procedure RemoveNumber(var first: itemptr);
var
    tmp: itemptr;
begin
    tmp := first;
    first := first^.next;
    dispose(tmp)
end;

procedure PrintOutNumber(var first: itemptr);
begin
    write(first^.c);
    RemoveNumber(first)
end;

procedure InitList(var init, first, last: itemptr);
begin
    init := nil;
    first := nil;
    last := nil
end;

procedure FillList(var init: itemptr);
var
    c, PrevC: char;
    first, last: itemptr;
begin
    InitList(init, first, last);
    c := ' ';
    PrevC := c;
    while (c <> #10) and (c <> #13) do
    begin
        read(c);
        if IsNumber(c) and (not IsNumber(PrevC)) then
            AddNumber(first, last, true, c)
        else if IsNumber(c) then
            AddNumber(first, last, false, c);
        if init = nil then
            init := first;
        PrevC := c
    end
end;

function CalculateMaxCountInList(var init: itemptr): integer;
var
    tmp: itemptr;
begin
    CalculateMaxCountInList := 0;
    tmp := init;
    while tmp <> nil do
    begin
        if tmp^.count > CalculateMaxCountInList then
            CalculateMaxCountInList := tmp^.count;
        tmp := tmp^.next
    end
end;

procedure PrintOutList(var init: itemptr; max: integer);
var
    tmp: itemptr;
begin
    tmp := init;
    while tmp <> nil do
    begin
        if tmp^.count = max then
        begin
            PrintOutNumber(tmp);
            while (tmp <> nil) and (tmp^.count = 0) do
                PrintOutNumber(tmp);
            write(' ')
        end
        else
            RemoveNumber(tmp)
    end;
    writeln
end;

var
    init: itemptr;
    max: integer;
begin
    while not SeekEof do
    begin
        FillList(init);
        max := CalculateMaxCountInList(init);
        PrintOutList(init, max)
    end
end.
