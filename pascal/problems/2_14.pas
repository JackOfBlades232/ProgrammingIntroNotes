program Problem2_14; { 2_14.pas }
const
    FillSymbol = '*';
    EmptySymbol = ' ';
    MinHeight = 5;

procedure PrintChars(count: integer; symbol: char);
var
	i: integer;
begin
	for i := 1 to count do
		write(symbol)
end;

procedure PrintZLine(SpacesCount: integer; symbol1, symbol2: char);
begin
    PrintChars(SpacesCount, symbol1);
    writeln(symbol2)
end;

procedure PrintZ(height: integer; symbol1, symbol2: char);
var
    i: integer;
begin
    for i := 1 to height do
    begin
        if (i = 1) or (i = height) or (i = height div 2 + 1) then
        begin
            PrintChars(height, symbol2);
            writeln
        end
        else
            PrintZLine(height - i, symbol1, symbol2)
    end
end;

var
    height: integer;
begin
    write('Input Z height: ');
    readln(height);
    while (height < MinHeight) or (height mod 2 = 0) do
    begin
        write('Height must be positive and odd, input another value: ');
        readln(height)
    end;
    PrintZ(height, EmptySymbol, FillSymbol)
end.
