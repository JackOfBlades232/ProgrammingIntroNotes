program Problem2_42; { 2_42.pas }
type
    itemptr = ^item;
    item = record
        c: char;
        next: itemptr;
    end;

function CharIsNotSpace(c: char): boolean;
begin
    CharIsNotSpace := (c <> ' ') and (c <> #9) and
        (c <> #10) and (c <> #13)
end;

procedure LinkWords(var last, first: itemptr);
begin
    if (last = nil) or (last^.next <> nil) then
    begin
        writeln('Unable to link words');
        halt(1)
    end;
    last^.next := first
end;

procedure AddLetter(var first, last: itemptr; c: char);
var
    tmp: itemptr;
begin
    new(tmp);
    tmp^.c := c;
    tmp^.next := nil;
    if last <> nil then
        last^.next := tmp
    else
        first := tmp;
    last := tmp
end;

var
    first, last, SecondFirst: itemptr;
    c, PrevC: char;
begin
    first := nil;
    last := nil;
    SecondFirst := nil;
    while not SeekEof do
    begin
        c := ' ';
        PrevC := ' ';
        while (c <> #10) and (c <> #13) do
        begin
            read(c);
            if (not CharIsNotSpace(c)) and CharIsNotSpace(PrevC) then
            begin
                AddLetter(first, last, ' ');
                LinkWords(last, SecondFirst);
                SecondFirst := first;
                first := nil;
                last := nil
            end
            else if CharIsNotSpace(c) then
                AddLetter(first, last, c);
            PrevC := c
        end;
        if first <> nil then
            LinkWords(last, SecondFirst)
        else
            first := SecondFirst;
        while first <> nil do
        begin
            write(first^.c);
            last := first;
            first := first^.next;
            dispose(last)
        end;
        writeln;
        first := nil;
        last := nil;
        SecondFirst := nil
    end
end.
        
        
