program GenerateTest; { generate_test.pas }
uses crt;
const
    cap = 1000;
    MinOrd = 48;
    MaxOrd = 122;
var
    i, code: integer;
begin
    randomize;
    for i := 1 to random(cap) do
    begin
        code := random(2);
        if code = 0 then
            write('+ ')
        else
            write('- ');
        writeln(chr(random(MaxOrd - MinOrd) + MinOrd));
        if code = 0 then
            writeln(1);
        {$IFDEF DEBUG}
        writeln('~')
        {$ENDIF}
    end;
    {$IFDEF TEST}
    writeln('!');
    {$ENDIF}
    writeln('#')
end.
