program Problem2_55; { 2_55.pas }
uses sysutils;
const
    NameBufSize = 60;
    CntBufSize = 4;

procedure TryOpenFile(var f: file; name: string);
begin
    {$I-}
    assign(f, name);
    reset(f, 1);
    if IOResult <> 0 then
    begin
        writeln(ErrOutput, 'Couldn''t open ', name); 
        halt(1)
    end
end;

procedure TryOpenElseCreateFile(var f: file; name: string);
begin
    {$I-}
    assign(f, name);
    reset(f, 1);
    if IOResult <> 0 then
        rewrite(f, 1)
end;

procedure CheckForWriteError(ReadRes, WriteRes: longint);
begin
    if ReadRes <> WriteRes then
        writeln(ErrOutput, 'Couldn''t write to file')
end;

procedure ListFile(filename: string);
var
    f: file;
    name: string;
    cnt, ReadRes: longint;
begin
    TryOpenFile(f, filename);
    while true do
    begin
        BlockRead(f, name, NameBufSize, ReadRes);
        BlockRead(f, cnt, CntBufSize, ReadRes);
        if ReadRes = 0 then
            break;
        writeln(name, ' ', cnt)
    end;
    close(f)
end;

procedure PerformQuery(filename: string; query: string);
label
    CloseFile;
var
    f: file;
    name: string;
    cnt, ReadRes: longint;
    res: integer;
begin
    TryOpenFile(f, filename);
    while true do
    begin
        BlockRead(f, name, NameBufSize, ReadRes);
        BlockRead(f, cnt, CntBufSize, ReadRes);
        if ReadRes = 0 then
            break;
        res := CompareStr(name, query);
        if res = 0 then
        begin
            writeln(cnt);
            goto CloseFile
        end
    end;
    writeln(0);
CloseFile:
    close(f)
end;

procedure PerformAdd(filename: string; query: string);
label
    CloseFile;
var
    f: file;
    name: string;
    cnt, ReadResName, ReadResCnt, WriteRes: longint;
    position: int64;
    res: integer;
begin
    TryOpenElseCreateFile(f, filename);
    position := 0;
    while true do
    begin
        BlockRead(f, name, NameBufSize, ReadResName);
        position := position + NameBufSize;
        if ReadResName = 0 then
            break;
        res := CompareStr(name, query);
        BlockRead(f, cnt, CntBufSize, ReadResCnt);
        if ReadResCnt = 0 then
            break;
        if res = 0 then
        begin
            {$IFDEF DEBUG}
            writeln('DEBUG: incrementing item: ', name,
                ', pointer position: ', position);
            {$ENDIF}
            cnt := cnt + 1;
            seek(f, position);
            BlockWrite(f, cnt, CntBufSize, WriteRes);
            CheckForWriteError(ReadResCnt, WriteRes);
            goto CloseFile
        end;
        position := position + CntBufSize
    end;
    cnt := 1;
    BlockWrite(f, query, NameBufSize, WriteRes);
    BlockWrite(f, cnt, CntBufSize, WriteRes);
CloseFile:
    close(f)
end;

var
    res: integer;
begin
    if ParamCount < 2 then
    begin
        writeln(ErrOutput, 'Specify file and command!');
        halt(1)
    end;
    res := CompareStr(ParamStr(2), 'list');
    if res = 0 then
    begin
        ListFile(ParamStr(1));
        halt(0)
    end;
    if ParamCount < 3 then
    begin
        writeln(ErrOutput, 'Specify item!');
        halt(1)
    end;
    res := CompareStr(ParamStr(2), 'query');
    if res = 0 then
    begin
        PerformQuery(ParamStr(1), ParamStr(3));
        halt(0)
    end;
    res := CompareStr(ParamStr(2), 'add');
    if res = 0 then
    begin
        PerformAdd(ParamStr(1), ParamStr(3));
        halt(0)
    end;
    writeln(ErrOutput, 'Unknown command: ', ParamStr(2))
end.
