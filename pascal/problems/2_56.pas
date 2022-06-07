program Problem2_56; { 2_56.pas }
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

procedure CheckForWriteError(ReadRes, WriteRes: longint);
begin
    if ReadRes <> WriteRes then
        writeln(ErrOutput, 'Couldn''t write to file')
end;

procedure MergeRecord(var f1, f2, OutFile: file;
    var index, position: longint; IsLeft: boolean; var ok: boolean); 
{ Nested loop implementation }
label
    Increment;
var
    name1, name2: string;
    cnt1, cnt2, ReadRes1Cnt, ReadRes2Cnt,
        ReadRes1Name, ReadRes2Name, WriteRes, res: longint;
begin
    seek(f1, index * (NameBufSize + CntBufSize));
    BlockRead(f1, name1, NameBufSize, ReadRes1Name);
    BlockRead(f1, cnt1, CntBufSize, ReadRes1Cnt);
    if ReadRes1Cnt <> CntBufSize then
    begin
        ok := false;
        exit
    end;
    res := 1;
    seek(f2, 0);
    while res <> 0 do
    begin
        BlockRead(f2, name2, NameBufSize, ReadRes2Name);
        BlockRead(f2, cnt2, CntBufSize, ReadRes2Cnt);
        if ReadRes2Cnt <> ReadRes1Cnt then
            break;
        res := CompareStr(name1, name2)
    end;
    {$IFDEF DEBUG}
    writeln('DEBUG: name comparison result: ', res);
    {$ENDIF}
    if res = 0 then
        if IsLeft then
            cnt1 := cnt1 + cnt2
        else
            goto Increment;
    seek(OutFile, position);
    BlockWrite(OutFile, name1, NameBufSize, WriteRes);
    CheckForWriteError(ReadRes1Name, WriteRes);
    BlockWrite(OutFile, cnt1, CntBufSize, WriteRes);
    CheckForWriteError(ReadRes1Cnt, WriteRes);
    position := position + NameBufSize + CntBufSize;
Increment:
    index := index + 1;
    ok := true
end;

var
    f1, f2, OutFile: file; 
    ok: boolean;
    index, position: longint;
begin
    if ParamCount < 3 then
    begin
        writeln(ErrOutput, 'Specify 2 input files and out file');
        halt(1)
    end;
    TryOpenFile(f1, ParamStr(1));
    TryOpenFile(f2, ParamStr(2));
    assign(OutFile, ParamStr(3));
    rewrite(OutFile, 1);
    position := 0;
    index := 0;
    repeat
        MergeRecord(f1, f2, OutFile, index, position, true, ok)
    until not ok;
    index := 0;
    repeat
        MergeRecord(f2, f1, OutFile, index, position, false, ok)
    until not ok;
    close(f1);
    close(f2);
    close(OutFile)
end.
