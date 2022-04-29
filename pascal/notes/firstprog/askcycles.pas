program askcycles;
var
	year: integer;
begin
	writeln('Type in your birth year: ');
	readln(year);
	while (year < 1900) or (year > 2020) do
	begin
		writeln('Type in your birth year: ');
		readln(year);
	end;
	writeln('Your birth year is ', year);
end.
