program GoodNumbers; { good_num.pas }
type
	itemptr = ^item;
	item = record
		data: integer;
		next: itemptr;
	end;
var
	first, tmp: itemptr;
	n: integer;
begin
	first := nil; { so that you can start while loop straight away }
	while not SeekEof do
	begin
		read(n);
		new(tmp);
		tmp^.data := n;
		tmp^.next := first;
		first := tmp
	end;
	tmp := first;
	while tmp <> nil do
	begin
		write(tmp^.data, ' ');
		tmp := tmp^.next
	end;
	writeln;
	while first <> nil do { delete the list }
	begin
		tmp := first^.next;
		dispose(first);
		first := tmp
	end
end.
