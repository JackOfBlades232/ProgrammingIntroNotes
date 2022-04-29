program Problem2_20; { 2_20.pas }

function CharIsSpace(c: char): boolean;
begin
    CharIsSpace := (c = ' ') or (c = #9) or (c = #10) or (c = #13)
end;

function CharIsSymbol(c: char): boolean;
begin
    CharIsSymbol := (c >= '!') and (c <= '~') 
end;

var
    c, PrevC: char;
begin
    c := ' ';
    PrevC := c;
    while not eof do
    begin
        read(c);
        if CharIsSymbol(c) and (not CharIsSymbol(PrevC)) then
            write('(');
        if (not CharIsSymbol(c)) and CharIsSymbol(PrevC) then
            write(')');
        if CharIsSymbol(c) or CharIsSpace(c) then
            write(c);
        PrevC := c
    end
end.
