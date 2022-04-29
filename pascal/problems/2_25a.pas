program Problem2_25a; { 2_25a.pas }
const
    StarSymbol = '*';
    EmptySymbol = ' ';

procedure PrintChars(count: integer; symbol: char);
var
	i: integer;
begin
	for i := 1 to count do
		write(symbol)
end;

procedure PrintLine(count1, count2: integer; symbol1, symbol2: char);
begin
    PrintChars(count1, symbol1);
    PrintChars(count2, symbol2);
    PrintChars(count1, symbol1);
    writeln
end;

procedure PrintStar(height: integer; symbol1, symbol2: char);
var
    i, HalfHeight: integer;
begin
    HalfHeight := height div 2;
    for i := 1 to height do
    begin
        if i <= HalfHeight then
            PrintLine(HalfHeight - i + 2, 1 + 2 * (i - 1),
                symbol1, symbol2)
        else
            PrintLine(i - HalfHeight, 1 + 2 * (height - i),
                symbol1, symbol2);
    end
end;

procedure PrintFilledStar(height: integer);
begin
    PrintStar(height, EmptySymbol, StarSymbol)
end;

procedure PrintEmptyStar(height: integer);
begin
    PrintChars(height + 2, StarSymbol);
    writeln;
    PrintStar(height, StarSymbol, EmptySymbol);
    PrintChars(height + 2, StarSymbol);
    writeln
end;

procedure ReadIntegerAndRaiseError(value, name: string; var out: integer);
var
    code: integer;
begin
    val(value, out, code);
    if code <> 0 then
    begin
        writeln(name, ' param must be integer!');
        halt(1)
    end
end;

var
    height, code: integer;
begin
    if ParamCount < 2 then
    begin
        writeln('Must be at least 2 parameters!');
        halt(1)
    end;
    ReadIntegerAndRaiseError(ParamStr(1), 'First', code);
    ReadIntegerAndRaiseError(ParamStr(2), 'Second', height);
    if (height <= 0) or (height mod 2 = 0) then
    begin
        writeln('Height must be positive and odd!');
        halt(1)
    end;
    if code = 0 then
        PrintFilledStar(height)
    else
        PrintEmptyStar(height)
end.
