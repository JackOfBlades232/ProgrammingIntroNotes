program bitwise;
var
	x, y: integer;
begin
	x := 42;
	y := 166;
	writeln('Not ', x, ': ', not x);
	writeln(x, ' and ', y, ': ', x and y);
	writeln(x, ' or ', y, ': ', x or y);
	writeln(x, ' xor ', y, ': ', x xor y);
	writeln('shift ', x, ' 1 step left: ', x shl 1);
	writeln('shift ', x, ' 1 step right: ', x shr 1)
end.
