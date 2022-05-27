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

procedure ReadStrToReal(s: string; var out: real);
var
    code: integer;
begin
    val(s, out, code);
    if code <> 0 then
    begin
        writeln(ErrOutput, 'Param must be a real number!');
        halt(1)
    end
end;

procedure PrintCharSeq(var f: text; c: char; cnt: integer);
var
    i: integer;
begin
    for i := 1 to cnt do
        write(f, c)
end;

procedure PrintOutReal(var f: text; num: real;
    FieldWidth, DecimalCount: integer; TruncateZeros: boolean);
label
    Printing;
var
    s: string;
    ExcessCounter, i: integer;
begin
    str(num:FieldWidth:DecimalCount, s);
    ExcessCounter := 0;
    if not TruncateZeros then
        goto Printing;
    for i := length(s) downto 1 do
        if (s[i] >= '1') and (s[i] <= '9') then
            break
        else if s[i] = '.' then
        begin
            ExcessCounter := ExcessCounter + 1;
            break
        end
        else
            ExcessCounter := ExcessCounter + 1;
Printing:
    PrintCharSeq(f, ' ', ExcessCounter);
    write(f, copy(s, 1, length(s) - ExcessCounter))
end;

var
    f: text;
    angle, MaxAngle, step, radians, sine, cosine: real;
    DecimalCount, code: integer;
begin
    if ParamCount < 5 then
    begin
        writeln(ErrOutput,
            'Specify output file, min angle, max angle, step and decimal cnt');
        halt(1)
    end;
    ReadStrToReal(ParamStr(2), angle);
    ReadStrToReal(ParamStr(3), MaxAngle);
    ReadStrToReal(ParamStr(4), step);
    val(ParamStr(5), DecimalCount, code);
    if code <> 0 then
    begin
        writeln(ErrOutput, 'Decimal count must be integer!');
        halt(1)
    end;
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
        radians := Deg2Rad(angle);
        sine := sin(radians);
        cosine := cos(radians);
        PrintOutReal(f, angle, RealStrLen, DecimalCount, true);
        write(f, ' | ');
        PrintOutReal(f, sine, RealStrLen, DecimalCount, false);
        write(f, ' | ');
        PrintOutReal(f, cosine, RealStrLen, DecimalCount, false);
        write(f, ' | ');
        PrintOutReal(f, sine / cosine, RealStrLen, DecimalCount, false);
        write(f, ' | ');
        PrintOutReal(f, cosine / sine, RealStrLen, DecimalCount, false);
        writeln(f, ' |');
        angle := angle + step
    end;
    PrintCharSeq(f, '-', NumColumns * RealStrLen + ExcessSymbols + 1);
    writeln(f);
    close(f)
end.
