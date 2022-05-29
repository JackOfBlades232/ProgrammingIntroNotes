program Problem2_54; { solution.pas }
const
    MaxLongint = 2147483647;
    MinLongint = -2147483648;

procedure TryOpenLongintFile(var f: file; name: string);
begin
end;

var
    InFile: file of longint;
    OutFile: text;
    min, max, n: longint;
    cnt, i: integer;
begin
    {$I-}
    if ParamCount < 2 then
    begin
        writeln('Specify at least 1 input file and output file!');
        halt(1)
    end;
    assign(OutFile, ParamStr(ParamCount));
    rewrite(OutFile);
    for i := 1 to ParamCount - 1 do
    begin
        assign(InFile, ParamStr(i));
        reset(InFile);
        if IOResult <> 0 then
        begin
            writeln(ErrOutput, 'Couldn''t open file!');
            halt(1)
        end;
        min := MaxLongint;
        max := MinLongint;
        cnt := 0;
        while not eof(InFile) do
        begin
            {$IFDEF DEBUG}
            writeln('DEBUG: iteration passed');
            {$ENDIF}
            read(InFile, n);
            if n < min then
                min := n;
            if n > max then
                max := n; 
            cnt := cnt + 1
        end;
        write(OutFile, ParamStr(i), ' | numbers: ', cnt);
        if cnt > 0 then
            write(OutFile, ' min: ', min, ' max: ', max);
        writeln(OutFile);
        close(InFile)
    end;
    close(OutFile)
end.
