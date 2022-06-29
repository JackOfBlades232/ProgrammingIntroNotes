program join; { join.pas }
uses HashJoin;
begin
    if ParamCount < 3 then
    begin
        writeln(ErrOutput, 'Specify 2 files to join and out file');
        halt(1)
    end;
    PerformJoin(ParamStr(1), ParamStr(2), ParamStr(3))
end.
