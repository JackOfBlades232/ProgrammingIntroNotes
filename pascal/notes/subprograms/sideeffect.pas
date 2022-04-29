program sideeffect;
var
	gl: integer;

function Cube(x: integer): integer;
begin
	gl := 5;
	Cube := x * x * x
end;

begin
	writeln('Init val: ', gl);
	writeln('Cube of 3: ', Cube(3));
	writeln('Init val changed due to side effect: ', gl)
end.
