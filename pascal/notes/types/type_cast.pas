program TypeCast; { type_cast.pas }
var
	i, j: integer;
	r: real;
	c: char;
	s: string;
begin
	r := 15.75;
	i := trunc(r);
	j := round(r);
	writeln(r, ' ', i, ' ', j);
	writeln('byte(''5'') = integer(''5'') = ', byte('5'));
	c := 'Z';
	writeln(string(c));
	s := 'ABC';
	s := s + c;
	writeln(s);
end.
