program Problem2_48; { solution.pas }
var
    f: text;
    LineCount: integer;
begin
    {$I-}
    if ParamCount < 1 then
    begin
        writeln('Specify source file name');
        halt(1)
    end;
    assign(f, ParamStr(1));
    reset(f);
    if IOResult <> 0 then
    begin
        writeln('Couldn''t open source file');
        halt(1)
    end;
    LineCount := 0;
    while not SeekEof(f) do
    begin
        readln(f);
        LineCount := LineCount + 1
    end;
    writeln('This file has ', LineCount, ' lines');
    close(f)
end.
