program Problem2_15; { 2_15.pas }
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

procedure PrintZLine(SpacesCount: integer);
begin
    PrintChars(SpacesCount, EmptySymbol);
    write(FillSymbol)
end;

procedure PrintRegularLine(SpacesCountInit, SpacesCount, ZCount: integer);
var
    i: integer;
begin
    PrintZLine(SpacesCountInit);
    for i := 1 to ZCount - 1 do
        PrintZLine(SpacesCount);
    writeln
end;

procedure PrintFullLine(ZCount, height: integer);
var
    i: integer;
begin
    for i := 1 to ZCount do
    begin
        PrintChars(height, FillSymbol);
        write(EmptySymbol)
    end;
    writeln
end;

procedure PrintPrefix(count, height: integer);
var
    i: integer;
begin
    for i := 1 to count do
        PrintChars(height + 1, EmptySymbol)
end;

procedure PrintFirstSection(count, height: integer);
var
    i: integer;
begin
    PrintFullLine(1, height);
    for i := 1 to height div 2 - 1 do
        PrintRegularLine(height - i - 1, 0, 1);
    if count > 1 then
        PrintFullLine(2, height)
    else
        PrintFullLine(1, height)
end;

procedure PrintMainPart(count, height: integer);
var
    i, level, SpaceCount, NumZInLine, NumZInFullLine: integer;
begin
    SpaceCount := height + height div 2;
    NumZInLine := 2;
    NumZInFullLine := 3;
    for level := 1 to count do
    begin
        if level = count - 1 then
            NumZInFullLine := 2;
        if level = count then
        begin
            NumZInLine := 1;
            NumZInFullLine := 1
        end;
        for i := 1 to height div 2 - 1 do
        begin
            PrintPrefix(level - 1, height);
            PrintRegularLine(height div 2 - i, SpaceCount, NumZInLine)
        end;
        PrintPrefix(level - 1, height);
        PrintFullLine(NumZInFullLine, height)
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
    count, height: integer;
begin
    if ParamCount < 2 then
    begin
        writeln('Must be at least 2 parameters!');
        halt(1)
    end;
    ReadIntegerAndRaiseError(ParamStr(1), 'First', height);
    if (height < MinHeight) or (height mod 2 = 0) then
    begin
        writeln('Height must be at least ', MinHeight, ' and odd!');
        halt(1)
    end;
    ReadIntegerAndRaiseError(ParamStr(2), 'Second', count);
    if count < 1 then
    begin
        writeln('Number of Z''s must be positive!');
        halt(1)
    end;
    PrintFirstSection(count, height);
    PrintMainPart(count, height)
end.
