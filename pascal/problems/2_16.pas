program Problem2_16; { 2_16.pas }

function CharIsLetter(c: char): boolean;
begin
    CharIsLetter := ((c >= 'A') and (c <= 'Z')) or ((c >= 'a') and (c <= 'z'))
end;

var
    i, order: integer;
    phrase: string;
    AsciiArray: array [0..255] of integer;
begin
    for i := 0 to 255 do
        AsciiArray[i] := 0;
    readln(phrase);
    for i := 1 to length(phrase) do
    begin
        order := ord(phrase[i]);
        AsciiArray[order] := AsciiArray[order] + 1
    end;
    for i := 1 to length(phrase) do
    begin
        order := ord(phrase[i]);
        if (AsciiArray[order] > 1) and CharIsLetter(phrase[i]) then 
            write(phrase[i]);
        AsciiArray[order] := 0
    end;
    writeln
end.
