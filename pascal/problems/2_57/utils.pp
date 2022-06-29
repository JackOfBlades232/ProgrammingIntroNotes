unit utils; { utils.pp }
interface
const
    MaxNumRecords = 10000000;
    HashBase = 0;
    HashMultiplier = 31;
    NameBufSize = 60;
    CntBufSize = 4;

procedure TryOpenFile(var f: file; name: string);
procedure CreateHashTable(var f: file; NumRecords: longint);
procedure TryOpenElseCreateFile(var f: file; name: string);
procedure CheckForWriteError(ReadRes, WriteRes: longint);
function HashFunction(name: string; NumRecords: longint): longint;

implementation

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

end.
