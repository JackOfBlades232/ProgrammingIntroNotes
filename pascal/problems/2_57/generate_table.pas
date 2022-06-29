program GenerateTable; { generate_table.pas }
uses HashTable;
const
    RecordSize = 16;
    AllLetters = 'qwertyuiopasdfghjklzxcvbnm';

procedure GenerateRandomString(var s: string; len: integer);
var
    i: integer;
begin
    s := '';
    for i := 1 to len do
    {$IFDEF DEBUG}
    begin
    {$ENDIF}
        insert(AllLetters[random(length(AllLetters) - 1) + 1], s, i);
    {$IFDEF DEBUG}
        writeln('DEBUG: generated char -- ', s[i]);
    end;
    writeln('DEBUG: generated string -- ', s);
    {$ENDIF}
end;

var
    name: string;
    i, NumRecords: longint;
    code: word;
begin
    if ParamCount < 2 then
    begin
        writeln(ErrOutput, 'Specify file name and num of entries');
        halt(1)
    end;
    val(ParamStr(2), NumRecords, code);
    if code <> 0 then
    begin
        writeln(ErrOutput, 'Second param must be integer');
        halt(1)
    end;
    randomize;
    for i := 1 to NumRecords do
    begin
        GenerateRandomString(name, RecordSize);
        {$IFDEF DEBUG}
        writeln('DEBUG: generated name -- ', name);
        {$ENDIF}
        PerformAdd(ParamStr(1), name, 1)
    end
end.
