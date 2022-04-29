program PointerToPointer; { pointer_to_pointer.pas }
type
	itemptr = ^item;
	item = record
		next: itemptr;
		data: integer;
	end;
var
	first, tmp: itemptr;
	pp: ^itemptr;
	n: integer;
begin
	{$B-} { use lazy semantic (change to + for non-lazy) }
	first := nil;
	while not SeekEof do
	begin
		read(n);
		if first = nil then
		begin
			new(first);
			tmp := first
		end
		else
		begin
			new(tmp^.next);
			tmp := tmp^.next
		end;
		tmp^.data := n;
		tmp^.next := nil
	end;
	pp := @first;
	while pp^ <> nil do
	begin
		if pp^^.data < 0 then
		begin
			tmp := pp^; { delete elem from linked list }
			n := tmp^.data;
			pp^ := tmp^.next;
			dispose(tmp);
			new(tmp); { insert elem into linked list }
			tmp^.next := pp^;
			tmp^.data := -n;
			pp^ := tmp;
		end
		else
			pp := @(pp^^.next)
	end;
	while first <> nil do
	begin
		tmp := first;
		writeln(tmp^.data);
		first := tmp^.next;
		dispose(tmp)
	end

	{ insert into sorted linked list:
	pp := @first;
	while (pp^ <> nil) and (pp^^.data < n) do
		pp := @(pp^^.next);
	new(tmp);
	tmp^.next := pp^;
	tmp^.data := n;
	pp^ := tmp; }
end.

