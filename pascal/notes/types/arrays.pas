program arrays;
type
	real100 = array [1..100] of real;
	colors = (red, green, blue);
	RGB = array [colors] of integer;
	int5 = array [1..5] of integer;
var
	a, b: real100;
	i: integer;
	z: RGB;
	x: int5;
begin
	a[1] := 1;
    for i := 2 to 100 do
		a[i] := a[i - 1] / 2;
	for i := 1 to 100 do
		b[i] := sin(a[i]);
	for i := 1 to 100 do
		writeln(a[i], ' ', b[i]);
	z[red] := 12;
	z[green] := 128;
	z[blue] := 32;
	writeln ('Color RGB: ', z[red], ' ', z[green], ' ', z[blue])
	x := (9, 8, 5)
end.
