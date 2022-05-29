program InputGen; { input_generator.pas }
var
    f: file of longint;
    n: longint;
begin
    if ParamCount < 1 then
    begin
        writeln('Specify input file name!');
        halt(1)
    end;
    assign(f, ParamStr(1));
    rewrite(f);
    while not SeekEof do
    begin
        read(n);
        write(f, n)
    end;
    close(f)
end.
