program Problem2_46; { solution.pas }
const
    source = 'humpty.txt';
var
    f, out: text;
    c: char;
begin
    {$I-}
    assign(f, source);
    reset(f);
    if IOResult <> 0 then
    begin
        writeln(ErrOutput, 'Couldn''t open source file');
        halt(1)
    end;
    if ParamCount < 1 then
    begin
        writeln('Specify output file name');
        halt(1)
    end;
    assign(out, ParamStr(1));
    rewrite(out);
    while not eof(f) do
    begin
        read(f, c);
        write(out, c)
    end;
    close(f);
    close(out)
end.
