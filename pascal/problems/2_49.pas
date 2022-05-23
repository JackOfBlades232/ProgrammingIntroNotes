program Problem2_49; { 2_49.pas }
var
    f: text;
    c: char;
begin
    if ParamCount < 1 then
    begin
        writeln('Specify output file name');
        halt(1)
    end;
    assign(f, ParamStr(1));
    rewrite(f);
    while not eof do
    begin
        read(c);
        write(f, c)
    end;
    close(f)
end.
