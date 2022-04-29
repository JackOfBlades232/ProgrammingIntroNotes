program starart;
var
	height, n, m: integer;
begin
	readln(height);
	for n := 1 to height + 1 do
	begin
		for m := 1 to height + 1 - n do
			write(' ');
		if n > 1 then
		begin
			write('*');
			for m := 1 to 1 + (n - 2) * 2 do
				write(' ');
		end;
		writeln('*');	
	end;
	for n := height downto 1 do
	begin
		for m := 1 to height + 1 - n do
			write(' ');
		if n > 1 then
		begin
			write('*');
			for m := 1 to 1 + (n - 2) * 2 do
				write(' ');
		end;
		writeln('*');	
	end
end.
