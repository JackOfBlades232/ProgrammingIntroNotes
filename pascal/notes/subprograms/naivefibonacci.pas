program naivefibonacci;

function Fibonacci(n: integer): longint;
var
	i: integer;
	p, q, r: longint;
begin
	if n <= 0 then
		Fibonacci := 0
	else
	begin
		q := 0;
		r := 1;
		for i := 2 to n do
		begin
			p := q;
			q := r;
			r := p + q;
		end;
		Fibonacci := r;
	end
end;

var
	n: integer;
begin
	write('Input n: ');
	readln(n);
	writeln('The ', n, '-th Fibonacci number is ', Fibonacci(n))
end.
