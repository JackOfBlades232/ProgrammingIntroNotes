program exitprogram;

procedure Quadratic(a, b, c: real; var ok: boolean; var x1, x2: real);
var
	d: real;
begin
	ok := false;
	if a = 0 then
		exit;
	d := b * b - 4 * a * c;
	if d < 0 then
		exit;
	d := sqrt(d);
	x1 := (-b + d) / (2 * a);
	x2 := (-b - d) / (2 * a);
	ok := true
end;

var
	a, b, c, x1, x2: real;
	ok: boolean;
	i: integer;
begin
	write('Input a: ');
	readln(a);
	write('Input b: ');
	readln(b);
	write('Input c: ');
	readln(c);
	Quadratic(a, b, c, ok, x1, x2);
	if ok then
		writeln('Roots: ', x1, ' ', x2)
	else
		writeln('No solution');
	for i := 1 to 10 do
	begin
		if i = 3 then
			continue;
		write(i);
		if i = 6 then
			break
	end;
	writeln;
	halt(0);
	write('Stooooooopp')
end.
