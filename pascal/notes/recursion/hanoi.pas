program hanoi; { hanoi.pas }

procedure solve(source, target, transit, n: integer);
begin
    if n = 0 then
        exit;
    solve(source, transit, target, n - 1);
    writeln(n, ': ', source, ' -> ', target);
    solve(transit, target, source, n - 1)
end;

const
    source = 1;
    target = 3;
    transit = 2;
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
    solve(source, target, transit, n)
end.
