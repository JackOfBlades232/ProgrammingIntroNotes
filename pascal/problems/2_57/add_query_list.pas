program AddQueryList; { add_query_list.pp }
uses sysutils, HashTable;
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
        PerformAdd(ParamStr(1), ParamStr(3), 1);
        halt(0)
    end;
    writeln(ErrOutput, 'Unknown command: ', ParamStr(2))
end.
