program gotopoints;

procedure PrintNumbersUpToHundred(n: integer);
label
	Quit, PreQuit;
var
	i: integer;
begin
	if n < 0 then
		goto Quit;
	for i := 1 to n do
	begin
		if (i > n) or (i > 100) then
			goto PreQuit;
		write(i, ' ')
	end;
	PreQuit:
		writeln;
	Quit:
		writeln('Execution finished')
end;

var
	n: integer;
begin
	write('Input n: ');
	readln(n);
	PrintNumbersUpToHundred(n)
end.
