program Problem2_12; { 2_12.pas }
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
            PrintLine(HalfHeight - i + 2, 1 + 2 * (i - 1), symbol1, symbol2)
        else
            PrintLine(i - HalfHeight, 1 + 2 * (height - i), symbol1, symbol2);
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

var
    height, code: integer;
begin
    write('Input 0 for regular star, or anything else for empty star: ');
    readln(code);
    write('Input star height: ');
    readln(height);
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
