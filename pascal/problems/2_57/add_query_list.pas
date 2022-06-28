program Problem2_55; { 2_55.pas }
uses sysutils;
const
    MaxNumRecords = 10000000;
    HashBase = 0;
    HashMultiplier = 31;
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

procedure CreateHashTable(var f: file; NumRecords: longint);
var
    zero, i, WriteRes: longint;
    EmptyName: string;
begin
    rewrite(f, 1);
    zero := 0;
    EmptyName := '';
    for i := 1 to NumRecords do
    begin
        BlockWrite(f, EmptyName, NameBufSize, WriteRes);
        BlockWrite(f, zero, CntBufSize, WriteRes)
    end
end;

procedure TryOpenElseCreateFile(var f: file; name: string);
begin
    {$I-}
    assign(f, name);
    reset(f, 1);
    if IOResult <> 0 then
        CreateHashTable(f, MaxNumRecords) 
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
        if cnt <= 0 then
            continue;
        writeln(name, ' ', cnt)
    end;
    close(f)
end;

function HashFunction(name: string; NumRecords: longint): longint;
var
    i: integer;
begin
    HashFunction := HashBase;
    i := 1;
    while name[i] <> #0 do
    begin
        if i > NameBufSize then
        begin
            HashFunction := 0;
            break
        end;
        HashFunction := HashFunction * HashMultiplier + ord(name[i]);
        while HashFunction > NumRecords do
            HashFunction := HashFunction - NumRecords;
        i := i + 1
    end
end;

procedure PerformQuery(filename: string; query: string);
label
    CloseFile;
var
    f: file;
    name: string;
    cnt, hash, NameHash, ReadRes: longint;
    res: integer;
begin
    TryOpenFile(f, filename);
    hash := HashFunction(query, MaxNumRecords);
    {$IFDEF DEBUG}
    writeln('DEBUG: query: ', query, ', hash: ', hash);
    {$ENDIF}
    if hash = 0 then
        writeln(0);
    seek(f, hash * (NameBufSize + CntBufSize));
    repeat
        BlockRead(f, name, NameBufSize, ReadRes);
        BlockRead(f, cnt, CntBufSize, ReadRes);
        if ReadRes = 0 then
            break;
        res := CompareStr(name, query);
        if res = 0 then
        begin
            writeln(cnt);
            goto CloseFile
        end;
        NameHash := HashFunction(name, MaxNumRecords)
    until hash <> NameHash;
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
    cnt, hash, ReadRes, WriteRes: longint;
    position: int64;
    res: integer;
begin
    TryOpenElseCreateFile(f, filename);
    hash := HashFunction(query, MaxNumRecords);
    {$IFDEF DEBUG}
    writeln('DEBUG: add: ', query, ', hash: ', hash);
    {$ENDIF}
    if hash = 0 then
        writeln(0);
    position := hash * (NameBufSize + CntBufSize);
    seek(f, position); 
    repeat
        BlockRead(f, name, NameBufSize, ReadRes);
        position := position + NameBufSize;
        BlockRead(f, cnt, CntBufSize, ReadRes);
        if ReadRes = 0 then
            break;
        res := CompareStr(name, query);
        if res = 0 then
        begin
            {$IFDEF DEBUG}
            writeln('DEBUG: incrementing item: ', name,
                ', pointer position: ', position);
            {$ENDIF}
            cnt := cnt + 1;
            seek(f, position);
            BlockWrite(f, cnt, CntBufSize, WriteRes);
            CheckForWriteError(ReadRes, WriteRes);
            goto CloseFile
        end;
        position := position + CntBufSize
    until cnt = 0;
    seek(f, position - (NameBufSize + CntBufSize));
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
