program starart;

procedure PrintChars(count: integer; symbol: char);
var
	i: integer;
begin
	for i := 1 to count do
		write(symbol)
end;

procedure PrintLineOfStar(height, n: integer);
begin
	PrintChars(height + 1 - n, ' ');
	if n > 1 then
	begin
		write('*');
		PrintChars((n - 2) * 2 + 1, ' ');
	end;
	writeln('*')
end;

var
	height, n: integer;
begin
	repeat
		write('Input diamond half-height: ');
		readln(height);
	until height > 0;
	for n := 1 to height + 1 do
		PrintLineOfStar(height, n);
	for n := height downto 1 do
		PrintLineOfStar(height, n)
end.
