program GoodDoubleNumber; { good_double_num.pas }
type
	itemptr = ^item;
	item = record
		next: itemptr;
		data: integer;
	end;
	
procedure WriteList(last: itemptr);
begin
	while last <> nil do
	begin
		write(last^.data, ' ');
		last := last^.next
	end;
	writeln
end;

var
	first, last: itemptr;
begin
	first := nil;
	while not SeekEof do
	begin
		if first = nil then
		begin
			new(first);
			last := first
		end
		else
		begin
			new(last^.next);
			last := last^.next
		end;
		read(last^.data);
		last^.next := nil
	end;
	WriteList(first);
	WriteList(first)
end.
