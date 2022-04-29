program ReverseNumbers; { reverse_nums.pas }
type
	numptr = ^num;
	num = record
		data: integer;
		next: numptr;
	end;
var
	current, precurrent: numptr;
begin
	{$I-}
	new(current);
	current^.next := nil;
	read(current^.data);
	if IOResult <> 0 then
	begin
		write('Invalid integer input');
		halt(1)
	end;
	new(precurrent);
	while not SeekEof do
	begin
		precurrent^.next := current;
		current := precurrent;
		read(current^.data);
		if IOResult <> 0 then
		begin
			write('Invalid integer input');
			halt(1)
		end;
		new(precurrent)
	end;
	while current <> nil do
	begin
		write(current^.data, ' ');
		precurrent := current;
		current := current^.next;
		dispose(precurrent)
	end;
	writeln
end.
