program postcycles;
var
	year: integer;
begin
	repeat
		readln(year)
	until (year >= 1900) and (year <= 2020);
	writeln('Year ', year);
	repeat
		year := year + 1;
		year := year + 2;
		writeln(year);
	until year > 2030
end.
