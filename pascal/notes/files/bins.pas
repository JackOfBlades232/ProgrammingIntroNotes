program bins; { bins.pas }

procedure CheckWriteAbility(filename: string);
begin
	if IOResult <> 0 then
	begin
		writeln('Cannot write to file ', filename);
		halt(1)
	end
end;

const
	filename = 'numbin.txt';
	start = 1000;
	step = 1001;
	count = 100;
var
	i: integer;
	n: longint;
	f: file of longint;
begin
	{$I-}
	if filemode <> 2 then { 2 if able to read and write, 0 if only able to read }
	begin
		writeln('Cannot write to typed files');
		halt(1)
	end;
	assign(f, filename);
	rewrite(f);
	if IOResult <> 0 then
	begin
		writeln('Cannot open file ', filename);
		halt(1)
	end;
	n := start;
	for i := 1 to count do
	begin
		write(f, n);
		CheckWriteAbility(filename);
		n := n + step
	end;
	seek(f, 50);
	read(f, n);
	writeln(n);
	seek(f, 50);
	write(f, n + step)
end.
