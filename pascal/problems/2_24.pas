program Problem2_24; { 2_24.pas }

procedure CheckMistypeError(code: word; name: string);
begin
    if code <> 0 then
    begin
        writeln(ErrOutput, name, ' argument is not a real number!');
        halt(1)
    end
end;

function IsNoZeroNum(c: char): boolean;
begin
    IsNoZeroNum := (c >= '1') and (c <= '9')
end;

var
    x, y, res: real;
    n, i, digits: integer;
    code: word;
    s, DigitsS: string;
begin
    if ParamCount < 2 then
    begin
        writeln('Must have at least 2 parameters!');
        halt(1)
    end;
    val(ParamStr(1), x, code);
    CheckMistypeError(code, 'First');
    val(ParamStr(2), y, code);
    CheckMistypeError(code, 'Second');
    val(ParamStr(3), n, code);
    CheckMistypeError(code, 'Third');
    res := x * y;
    str(res, s);
    for i := 1 to 3 do
        DigitsS[i] := s[length(s) - 3 + i];
    val(DigitsS, digits, code);
    str(res:digits:n, s);
    for i := length(s) downto 1 do
    begin
        if IsNoZeroNum(s[i]) or (i = 1) then
            break;
        s[i] := #0
    end;
    writeln(s)
end. 
