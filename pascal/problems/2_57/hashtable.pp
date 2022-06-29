unit HashTable; { hashtable.pp }
interface

procedure ListFile(filename: string);
procedure PerformQuery(filename: string; query: string);
procedure PerformAdd(filename: string; query: string; increment: longint);
procedure PerformAddToOpenFile(
    var f: file; query: string; hash, increment: longint);

implementation
uses sysutils, utils;

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

procedure PerformQuery(filename: string; query: string);
label
    CloseFile;
var
    f: file;
    name: string;
    cnt, hash, ReadRes: longint;
    res: integer;
begin
    TryOpenFile(f, filename);
    hash := HashFunction(query);
    {$IFDEF DEBUG}
    writeln('DEBUG: query: ', query, ', hash: ', hash);
    {$ENDIF}
    if hash = 0 then
        writeln(0);
    seek(f, hash * (NameBufSize + CntBufSize));
    while true do
    begin
        BlockRead(f, name, NameBufSize, ReadRes);
        BlockRead(f, cnt, CntBufSize, ReadRes);
        if (ReadRes = 0) or (cnt = 0) then
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

procedure PerformAddToOpenFile(
    var f: file; query: string; hash, increment: longint);
var
    name: string;
    cnt, ReadRes, WriteRes: longint;
    position, PrevPosition: int64;
    res: integer;
begin
    {$IFDEF DEBUG}
    writeln('DEBUG: add: ', query, ', hash: ', hash);
    {$ENDIF}
    if hash = 0 then
        exit;
    position := hash * (NameBufSize + CntBufSize);
    seek(f, position); 
    repeat
        PrevPosition := position;
        BlockRead(f, name, NameBufSize, ReadRes);
        position := position + NameBufSize;
        BlockRead(f, cnt, CntBufSize, ReadRes);
        if ReadRes = 0 then
        {$IFDEF DEBUG}
        begin
            writeln('DEBUG: hit eof while adding');
        {$ENDIF}
            break;
        {$IFDEF DEBUG}
        end;
        {$ENDIF}
        res := CompareStr(name, query);
        if res = 0 then
        begin
            {$IFDEF DEBUG}
            writeln('DEBUG: incrementing item: ', name,
                ', pointer position: ', position);
            {$ENDIF}
            cnt := cnt + increment;
            seek(f, position);
            BlockWrite(f, cnt, CntBufSize, WriteRes);
            CheckForWriteError(ReadRes, WriteRes);
            exit 
        end;
        position := position + CntBufSize
    until cnt = 0;
    seek(f, PrevPosition);
    cnt := increment;
    BlockWrite(f, query, NameBufSize, WriteRes);
    BlockWrite(f, cnt, CntBufSize, WriteRes)
end;

procedure PerformAdd(filename: string; query: string; increment: longint);
var
    f: file;
    hash: longint;
begin
    TryOpenElseCreateFile(f, filename);
    hash := HashFunction(query);
    PerformAddToOpenFile(f, query, hash, increment);
    close(f)
end;

end.
