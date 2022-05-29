program Problem2_53; { 2_53.pas }

procedure TryOpenFile(var f: text; name: string);
begin
    {$I-}
    assign(f, name);
    reset(f);
    if IOResult <> 0 then
    begin
        writeln(ErrOutput, 'Couldn''t open file!');
        halt(1)
    end
end;

var
    InFile, LinesOut: text;
    LengthsOut: file of integer;
    len: integer;
    c: char;
    LineEligible: boolean;
begin
    if ParamCount < 3 then
    begin
        writeln('Specify input file name and 2 out file names');
        halt(1)
    end;
    TryOpenFile(InFile, ParamStr(1));
    assign(LinesOut, ParamStr(2));
    rewrite(LinesOut);
    assign(LengthsOut, ParamStr(3));
    rewrite(LengthsOut);
    while not eof(InFile) do
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: iteration');
        {$ENDIF}
        len := 0;
        LineEligible := false;
        while not eoln(InFile) do
        begin
            read(InFile, c);
            if (len = 0) and (c = ' ') then
                LineEligible := true;
            if LineEligible then
                write(LinesOut, c);
            len := len + 1
        end;
        readln(InFile);
        if LineEligible then
            writeln(LinesOut);
        write(LengthsOut, len)
    end;
    close(InFile);
    close(LinesOut);
    close(LengthsOut)
end.
