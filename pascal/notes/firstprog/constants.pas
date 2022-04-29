program constants;
const
	hours = 24;
	minutes = 60;
	seconds = 60;
var
	x : integer;
begin
	readln(x);
	writeln('Days: ', x div (hours * minutes * seconds), ' Hours: ', (x div (minutes * seconds)) mod hours, ' Minutes: ', (x div seconds) mod minutes, ' Seconds: ', x mod seconds);
end.	
