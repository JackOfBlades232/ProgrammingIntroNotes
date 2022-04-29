program ComplexTypes; { complex_types.pas }
type
	period = record
		start: real;
		finish: real;
	end;
	period100 = array [1..100] of period;
	matrix5x5 = array [1..5] of array [1..5] of real;

procedure print(m: matrix5x5); { complex types only by name }
var
	i, j: integer;
begin
	for i := 1 to 5 do
	begin
		for j := 1 to 5 do
			write(m[i, j], ' ');
		writeln
	end
end;

var
	m: matrix5x5;
	i, j: integer;
begin
	m[2, 4] := 3;
	writeln(m[2][4]);
	for i := 1 to 5 do
		for j := 1 to 5 do
			m[i, j] := 3;
	print(m)
end. 
