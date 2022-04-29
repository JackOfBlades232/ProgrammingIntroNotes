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
    height: integer;
begin
    if ParamCount < 1 then
    begin
        writeln('Must be at least 1 parameter!');
        halt(1)
    end;
    ReadIntegerAndRaiseError(ParamStr(1), 'This', height);
    if (height < MinHeight) or (height mod 2 = 0) then
    begin
        writeln('Height must be positive and odd!');
        halt(1)
    end;
    PrintZ(height, EmptySymbol, FillSymbol)
end.
