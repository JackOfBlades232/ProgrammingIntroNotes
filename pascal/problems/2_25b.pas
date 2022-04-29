program Problem2_25b; { 2_25b.pas }
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

procedure PrintOneStarLine(count1, count2: integer;
    symbol1, symbol2: char);
begin
    PrintChars(count1, symbol1);
    write(symbol2);
    if count2 > 0 then
    begin
        PrintChars(count2, symbol1);
        write(symbol2)
    end;
    PrintChars(count1, symbol1);
    write(symbol1)
end;

procedure PrintLine(StarCount, count1, count2: integer;
    symbol1, symbol2: char);
var
    i: integer;
begin
    for i := 1 to StarCount do
        PrintOneStarLine(count1, count2, symbol1, symbol2);
    writeln
end;

procedure PrintStar(StarCount, height: integer; symbol1, symbol2: char);
var
    i, HalfHeight: integer;
begin
    HalfHeight := height div 2;
    for i := 1 to height do
    begin
        if i <= HalfHeight then
            PrintLine(StarCount, HalfHeight - i + 1,
                2 * (i - 1) - 1, symbol1, symbol2)
        else
            PrintLine(StarCount, i - HalfHeight - 1,
                2 * (height - i) - 1, symbol1, symbol2);
    end
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
    height, count: integer;
begin
    if ParamCount < 2 then
    begin
        writeln('Must be at least 2 parameters!');
        halt(1)
    end;
    ReadIntegerAndRaiseError(ParamStr(1), 'First', height);
    if (height <= 0) or (height mod 2 = 0) then
    begin
        writeln('Height must be positive and odd!');
        halt(1)
    end;
    ReadIntegerAndRaiseError(ParamStr(2), 'Second', count);
    if count < 1 then
    begin
        writeln('Count must be positive');
        halt(1)
    end;
    PrintStar(count, height, EmptySymbol, StarSymbol)
end.
