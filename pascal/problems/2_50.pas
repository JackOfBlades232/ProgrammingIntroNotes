program Problem2_50; { 2_50.pas }
const
    RealStrLen = 24;
    TrigNameLen = 3;
    TgNameLen = 2;
    AngleNameLen = 5;
    NumColumns = 5;
    ExcessSymbols = 13;
    DegInCircle = 360;
    Precision = 0.0001;

function Deg2Rad(angle: real): real;
begin
    Deg2Rad := 2 * angle * pi / DegInCircle
end;

procedure ReadStrToAngle(s: string; var out: real);
var
    code: integer;
begin
    val(s, out, code);
    if code <> 0 then
    begin
        writeln(ErrOutput, 'Param must be a real number!');
        halt(1)
    end;
    out := Deg2Rad(out)
end;

procedure PrintCharSeq(var f: text; c: char; cnt: integer);
var
    i: integer;
begin
    for i := 1 to cnt do
        write(f, c)
end;

var
    f: text;
    angle, MaxAngle, step, sine, cosine: real;
begin
    if ParamCount < 4 then
    begin
        writeln(ErrOutput,
            'Specify output file, min angle, max angle and step');
        halt(1)
    end;
    ReadStrToAngle(ParamStr(2), angle);
    ReadStrToAngle(ParamStr(3), MaxAngle);
    ReadStrToAngle(ParamStr(4), step);
    assign(f, ParamStr(1));
    rewrite(f);
    if (step <= 0) or (angle > MaxAngle) then
    begin
        writeln(ErrOutput, 
            'Str must be positive and angle mustnt exceed max angle');
        halt(1)
    end;
    write(f, ' angle');
    PrintCharSeq(f, ' ', RealStrLen - AngleNameLen);
    write(f, '| sin');
    PrintCharSeq(f, ' ', RealStrLen - TrigNameLen + 1);
    write(f, '| cos');
    PrintCharSeq(f, ' ', RealStrLen - TrigNameLen + 1);
    write(f, '| tg');
    PrintCharSeq(f, ' ', RealStrLen - TgNameLen + 1);
    write(f, '| ctg');
    PrintCharSeq(f, ' ', RealStrLen - TrigNameLen + 1);
    writeln(f, '|');
    while angle <= MaxAngle + Precision do
    begin
        PrintCharSeq(f, '-', NumColumns * RealStrLen + ExcessSymbols + 1);
        writeln(f);
        sine := sin(angle);
        cosine := cos(angle);
        writeln(f, angle, ' | ', sine, ' | ', cosine, ' | ',
            sine / cosine, ' | ', cosine / sine, ' |');
        angle := angle + step
    end;
    PrintCharSeq(f, '-', NumColumns * RealStrLen + ExcessSymbols + 1);
    writeln(f);
    close(f)
end.
