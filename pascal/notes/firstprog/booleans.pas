program booleans;
var
	x, y: integer;
	flag: boolean;
begin
	readln(x);
	readln(y);
	flag := (x > 1) and (y < 2);
	writeln(flag);
	flag := (x < 1) or (y > 2);
	writeln(flag);
	if flag then
		writeln('Good');
end.
