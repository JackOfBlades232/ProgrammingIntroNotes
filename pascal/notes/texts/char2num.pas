program char2num;

procedure ReadLongint(var success: boolean; var res: longint);
var
	c: char;
	number: longint;
	position: integer;
begin
	number := 0;
	position := 0;
	repeat
		read(c);
		position := position + 1;
	until (c <> ' ') and (c <> #10);
	while (c <> ' ') and (c <> #10) do
	begin
		if (c < '0') or (c > '9') then
		begin
			writeln('Unexpexted ''', c, ''' in pos: ', position);
			readln;
			success := false;
			exit
		end;
		number := number * 10 + ord(c) - ord('0');
		read(c);
		position := position + 1;
	end;
	res := number;
	success := true
end;

var
	x, y: longint;
	ok: boolean;
begin
	repeat
		write('Input x: ');
		ReadLongint(ok, x)
	until ok;
	repeat
		write('Input y: ');
		ReadLongint(ok, y)
	until ok;
	writeln(x, ' * ', y, ' = ', x * y)
end.
