program conditions;
var
	x: integer;
begin
	read(x);
	if x > 0 then
		writeln(x)
	else
		writeln(-x);
	writeln('x = x:', x = x);
	writeln('x <> (not equal) x + 1:', x <> (x + 1));
end.
