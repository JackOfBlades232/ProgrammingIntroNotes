program complex;
var
	a, b, t: integer;
begin
	readln(a);
	readln(b);
	if a > b then
	begin
		t := a;
		a := b;
		b := t;
	end;
	writeln(a, ' ', b);
end.
