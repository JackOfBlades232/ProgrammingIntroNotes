program hanoi;

procedure solve(source, target, interm, n: integer);
begin
    if n = 0 then
        exit;
    solve(source, interm, target, n-1);
    writeln(n, ': ',  source, ' -> ', target);
    solve(interm, target, source, n-1)
end;

var
    n, code: integer;
begin
    if ParamCount < 1 then
    begin
        writeln(ErrOutput, 'No parameters given');
        halt(1)
    end;
    val(ParamStr(1), n, code);
    if (code <> 0) or (n < 1) then
    begin
        writeln(ErrOutput, 'Invalid token count');
        halt(1)
    end;
    solve(1, 3, 2, n)
end.
