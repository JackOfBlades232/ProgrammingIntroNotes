unit utils; { utils.pp }
interface
const
    MaxNumRecords = 10000000;
    NameBufSize = 60;
    CntBufSize = 4;

procedure TryOpenFile(var f: file; name: string);
procedure CreateHashTable(var f: file);
procedure TryOpenElseCreateFile(var f: file; name: string);
procedure CheckForWriteError(ReadRes, WriteRes: longint);
function HashFunction(name: string): longint;

implementation
const
    HashBase = 0;
    HashMultiplier = 13;

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

procedure CreateHashTable(var f: file);
var
    zero, i, WriteRes: longint;
    EmptyName: string;
begin
    rewrite(f, 1);
    zero := 0;
    EmptyName := '';
    for i := 1 to MaxNumRecords do
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
        CreateHashTable(f)
end;

procedure CheckForWriteError(ReadRes, WriteRes: longint);
begin
    if ReadRes <> WriteRes then
        writeln(ErrOutput, 'Couldn''t write to file')
end;

function HashFunction(name: string): longint;
var
    i: integer;
    c: char;
begin
    HashFunction := HashBase;
    {$IFDEF DEBUG}
    write('DEBUG: string for hash: ', name, ', char ords: ');
    {$ENDIF}
    for i := 1 to length(name) do
    begin
        c := name[i];
        if (c = #0) or (i >= NameBufSize) then
            break;
        {$IFDEF DEBUG}
        write(c, ':', ord(c), ' ');
        {$ENDIF}
        HashFunction := HashFunction * HashMultiplier + ord(c);
        while HashFunction > MaxNumRecords do
            HashFunction := HashFunction - MaxNumRecords
    end;
    {$IFDEF DEBUG}
    writeln
    {$ENDIF}
end;

end.
