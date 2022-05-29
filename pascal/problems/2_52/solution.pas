program Problem2_52; { solution.pas }
type
    FiledataPtr = ^filedata;
    filedata = record
        name: string;
        idx, MaxCnt, MaxIdx: integer;
        next: FiledataPtr;
    end;

procedure TryOpenFile(var f: text; fp: FiledataPtr);
begin
    {$I-}
    assign(f, fp^.name);
    reset(f);
    if IOResult <> 0 then
    begin
        writeln(ErrOutput, 'Couldn''t open file!');
        dispose(fp);
        fp := nil;
        halt(1)
    end
end;

procedure ScanFile(name: string; var fp, init: FiledataPtr);
var
    tmp: FiledataPtr;
    f: text;
    c: char;
    cnt, idx: integer;
begin
    new(tmp);
    tmp^.name := name;
    TryOpenFile(f, tmp);
    if fp = nil then
    begin
        tmp^.idx := 1;
        init := tmp
    end
    else
    begin
        tmp^.idx := fp^.idx + 1;
        fp^.next := tmp
    end;
    tmp^.next := nil;
    tmp^.MaxCnt := -1;
    tmp^.MaxIdx := 0;
    idx := 0;
    {$IFDEF DEBUG}
    writeln('DEBUG: finished scan prep, iteration ', tmp^.idx);
    {$ENDIF}
    while not eof(f) do
    begin
        cnt := 0;
        idx := idx + 1;
        while not eoln(f) do
        begin
            {$IFDEF DEBUG}
            writeln('       read char, num ', cnt);
            {$ENDIF}
            read(f, c);
            cnt := cnt + 1
        end;
        if cnt > tmp^.MaxCnt then
        begin
            tmp^.MaxCnt := cnt;
            tmp^.MaxIdx := idx
        end;
        readln(f);
        {$IFDEF DEBUG}
        writeln('   finished scan iter ', idx);
        {$ENDIF}
    end;
    fp := tmp;
    close(f)
end;

procedure PrintLongestLine(fp: FiledataPtr);
var
    f: text;
    i: integer;
    c: char;
begin
    if fp = nil then
        exit;
    TryOpenFile(f, fp);
    for i := 1 to fp^.MaxIdx - 1 do
        readln(f);
    while not eoln(f) do
    begin
        read(f, c);
        write(c)
    end;
    close(f);
    writeln
end;

function FileWithLongestLineIndex(fp: FiledataPtr): integer;
var
    MaxLine: integer;
begin
    FileWithLongestLineIndex := 0;
    MaxLine := -1;
    while fp <> nil do
    begin
        if fp^.MaxCnt > MaxLine then
        begin
            MaxLine := fp^.MaxCnt;
            FileWithLongestLineIndex := fp^.idx
        end;
        fp := fp^.next
    end
end;

var
    init, cur: FiledataPtr;
    MaxFileIdx, i: integer;
begin
    init := nil;
    cur := nil;
    for i := 1 to ParamCount do
        ScanFile(ParamStr(i), cur, init);
    {$IFDEF DEBUG}
    writeln('DEBUG: Scanned all files');
    {$ENDIF}
    MaxFileIdx := FileWithLongestLineIndex(init);
    cur := init;
    while cur <> nil do
    begin
        if MaxFileIdx = cur^.idx then
            write('*');
        write(cur^.name, ':');
        PrintLongestLine(cur);
        init := cur;
        cur := cur^.next;
        dispose(init)
    end
end.
