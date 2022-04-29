program LinkedList; { linked_list.pas }
type
	itemptr = ^item;
	item = record
		data: integer;
		next: itemptr;
	end;
var
	first, current: itemptr;
begin
	new(first);
	first^.data := 25;
	new(first^.next);
	first^.next^.data := 36;
	new(first^.next^.next);
	first^.next^.next^.data := 49;
	first^.next^.next^.next := nil;
	current := first;
	while current <> nil do
	begin
		write(current^.data, ' ');
		current := current^.next
	end;
	writeln;
end.
