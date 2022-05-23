program Problem2_47; { solution.pas }
var
    f: text;
    c: char;
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
    while not eof(f) do
    begin
        read(f, c);
        write(c)
    end;
    close(f)
end.
