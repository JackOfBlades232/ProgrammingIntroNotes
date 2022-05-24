program GenerateTest; { generate_test.pas }
uses crt;
const
    cap = 1000;
    MinOrd = 65;
    MaxOrd = 90;
var
    i: integer;
begin
    randomize;
    for i := 1 to random(cap) do
    begin
        writeln('+ ', chr(random(MaxOrd - MinOrd) + MinOrd));
        writeln(1)
    end;
    writeln('!')
end.
