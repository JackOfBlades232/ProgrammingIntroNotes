program variables;
var
	x, y: integer;
	z: integer = 10;
	flag: boolean;
	d: word = 5;
begin
	writeln(x, ' ', y);
	x := 37;
	writeln(x);
	x := x + 5;
	writeln(x);
	flag := x > 123;
	writeln(flag);
end.
