program mutableparams;

procedure Powers(x: real; var quad, cube, fourth, fifth: real);
begin
	quad := x * x;
	cube := quad * x;
	fourth := cube * x;
	fifth := fourth * x
end;

var
	x, quad, cube, fourth, fifth: real;
begin
	write('Input x: ');
	readln(x);
	Powers(x, quad, cube, fourth, fifth);
	writeln('Square: ', quad, ' Cube: ', cube, ' Fourth: ', fourth, ' Fifth: ', fifth)
end.	
