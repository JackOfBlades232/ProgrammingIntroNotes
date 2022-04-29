program Problem2_20; { 2_20.pas }
const
    AllowedLen = 2;

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
    counter, i: integer;
    LastWordWasAllowed: boolean;
    LastWord: array [1..AllowedLen] of char;
begin
    c := ' ';
    PrevC := c;
    counter := 0;
    LastWordWasAllowed := false;
    while not eof do
    begin
        read(c);
        if (not CharIsSymbol(c)) and CharIsSymbol(PrevC) then
        begin
            if counter = AllowedLen then
            begin
                for i := 1 to AllowedLen do
                    write(LastWord[i]);
                LastWordWasAllowed := true
            end
            else
                LastWordWasAllowed := false;
            counter := 0
        end;
        if CharIsSymbol(c) then
        begin
            counter := counter + 1;
            if counter <= AllowedLen then
                LastWord[counter] := c
        end;
        if CharIsSpace(c) and LastWordWasAllowed then
            write(c);
        PrevC := c
    end
end.
