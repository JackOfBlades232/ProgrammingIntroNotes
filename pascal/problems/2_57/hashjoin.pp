unit HashJoin; { hashjoin.pp }
interface

procedure PerformJoin(filename1, filename2, OutFilename: string);

implementation
uses sysutils, utils, HashTable;

procedure JoinRecord(
    var f1, f2, OutFile: file; var index: longint; var ok: boolean
);
label
    Increment;
var
    name1, name2: string;
    cnt1, cnt2, ReadRes1, ReadRes2, res, hash: longint;
begin
    seek(f1, index * (NameBufSize + CntBufSize));
    BlockRead(f1, name1, NameBufSize, ReadRes1);
    BlockRead(f1, cnt1, CntBufSize, ReadRes1);
    if ReadRes1 <> CntBufSize then
    begin
        ok := false;
        exit
    end;
    if cnt1 = 0 then
        goto Increment;
    res := 1;
    hash := HashFunction(name1);
    seek(f2, hash * (NameBufSize + CntBufSize));
    while res <> 0 do
    begin
        BlockRead(f2, name2, NameBufSize, ReadRes2);
        BlockRead(f2, cnt2, CntBufSize, ReadRes2);
        if (ReadRes2 <> ReadRes1) or (cnt2 = 0) then
            break;
        res := CompareStr(name1, name2)
    end;
    {$IFDEF DEBUG}
    writeln('DEBUG: name comparison result: ', res);
    {$ENDIF}
    if res = 0 then
        cnt1 := cnt1 + cnt2;
    PerformAddToOpenFile(OutFile, name1, hash, cnt1);
Increment:
    ok := true;
    index := index + 1
end;

procedure XorJoinRecord(
    var f1, f2, OutFile: file; var index: longint; var ok: boolean
);
label
    Increment;
var
    name1, name2: string;
    cnt1, cnt2, ReadRes1, ReadRes2, res, hash: longint;
begin
    seek(f1, index * (NameBufSize + CntBufSize));
    BlockRead(f1, name1, NameBufSize, ReadRes1);
    BlockRead(f1, cnt1, CntBufSize, ReadRes1);
    if ReadRes1 <> CntBufSize then
    begin
        ok := false;
        exit
    end;
    if cnt1 = 0 then
        goto Increment;
    hash := HashFunction(name1);
    seek(f2, hash * (NameBufSize + CntBufSize));
    cnt2 := 1;
    while cnt2 <> 0 do
    begin
        BlockRead(f2, name2, NameBufSize, ReadRes2);
        BlockRead(f2, cnt2, CntBufSize, ReadRes2);
        if ReadRes2 <> ReadRes1 then
            break;
        res := CompareStr(name1, name2);
        if res = 0 then
            goto Increment
    end;
    {$IFDEF DEBUG}
    writeln('DEBUG: name comparison result: ', res);
    {$ENDIF}
    PerformAddToOpenFile(OutFile, name1, hash, cnt1);
Increment:
    ok := true;
    index := index + 1
end;

procedure PerformJoin(filename1, filename2, OutFilename: string);
{ Hash join with nested loop for collisions implementation }
var
    f1, f2, OutFile: file; 
    ok: boolean;
    index: longint;
begin
    TryOpenFile(f1, filename1);
    TryOpenFile(f2, filename2); 
    assign(OutFile, OutFilename);
    rewrite(OutFile, 1);
    seek(OutFile, 1);
    index := 0;
    repeat
        JoinRecord(f1, f2, OutFile, index, ok); 
    until not ok;
    index := 0;
    repeat
        XorJoinRecord(f2, f1, OutFile, index, ok); 
    until not ok;
    close(f1);
    close(f2);
    close(OutFile)
end;

end.
