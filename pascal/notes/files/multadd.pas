program MultAdd; { multadd.pas }
var
	sum, mul, n: integer;
	f: text;
begin
	{$I-}
	if ParamCount < 1 then
	begin
		writeln('Please specify the file name');
		halt(1)
	end;
	assign(f, ParamStr(1));
	reset(f);
	if IOResult <> 0 then
	begin
		writeln('Could not open ', ParamStr(1));
		halt(1)
	end;
	sum := 0;
	while not SeekEof(f) do
	begin
		mul := 1;
		while not SeekEoln(f) do
		begin
			read(f, n);
			mul := mul * n
		end;
		readln(f);
		sum := sum + mul
	end;
	close(f);
	writeln(sum)
end.
