program Problem2_42; { 2_42.pas }
type
    itemptr = ^item;
    item = record
        c: char;
        next, left, right: itemptr;
    end;

function CharIsNotSpace(c: char): boolean;
begin
    CharIsNotSpace := (c <> ' ') and (c <> #9) and
        (c <> #10) and (c <> #13)
end;

procedure AddLetter(var first, last, PreLast: itemptr; c: char);
label
    EndPoint;
var
    tmp, tmp1, tmp2: itemptr;
begin
    new(tmp);
    tmp^.c := c;
    tmp^.next := nil;
    tmp^.right := nil;
    tmp^.left := nil;
    if last <> nil then
        last^.next := tmp
    else
    begin
        first := tmp;
        if PreLast <> nil then
        begin
            PreLast^.right := first;
            first^.left := PreLast
        end;
        goto EndPoint
    end;
    tmp2 := PreLast;
    tmp1 := tmp;
    while (tmp2 <> nil) and (tmp2^.next = nil) do
    begin
        new(tmp1);
        tmp1^.c := ' ';
        tmp1^.next := nil;
        tmp2^.next := tmp1;
        tmp2^.right^.next^.left := tmp1;
        tmp1^.right := tmp2^.right^.next;
        tmp1^.left := nil;
        tmp2 := tmp2^.left
    end;
    if (tmp2 <> nil) then
    begin
        tmp2^.next^.right := tmp1;
        tmp1^.left := tmp2^.next
    end;
EndPoint:
    last := tmp;
    PreLast := last^.left
end;

var
    first, last, PreLast: itemptr;
    c, PrevC: char;
begin
    first := nil;
    last := nil;
    PreLast := nil;
    while not SeekEof do
    begin
        c := ' ';
        PrevC := ' ';
        first := nil;
        last := nil;
        PreLast := nil;
        while (c <> #10) and (c <> #13) do
        begin
            read(c);
            if (not CharIsNotSpace(c)) and CharIsNotSpace(PrevC) then
            begin
                PreLast := first;
                first := nil;
                last := nil
            end
            else if CharIsNotSpace(c) then
                AddLetter(first, last, PreLast, c);
            PrevC := c
        end;
        if first = nil then
            first := PreLast;
        while (first <> nil) and (first^.left <> nil) do
            first := first^.left;
        if first = nil then
            continue;
        PreLast := first;
        repeat
            first := PreLast;
            PreLast := first^.next;
            while first <> nil do
            begin
                write(first^.c);
                last := first;
                first := first^.right;
                dispose(last)
            end;
            writeln;
        until PreLast = nil
    end
end.
        
        
