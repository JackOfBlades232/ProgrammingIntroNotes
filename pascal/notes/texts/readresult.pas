program readresult;
var
	x, y: longint;
begin
	{$I-} { turns off errors and warnings }
	read(x, y);
	if IOResult = 0 then { last read proc success code }
		writeln(x * y)
	else
		writeln('Invalid input')
end.
