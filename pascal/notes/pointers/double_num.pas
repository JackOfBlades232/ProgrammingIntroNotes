program DoubleNumbers; { double_nums.pas }

procedure CheckInput;
begin
	if IOResult <> 0 then
	begin
		writeln('Incorrect input, should be int');
		halt(1)
	end
end;

type
	numptr = ^num;
	num = record
		next: numptr;
		data: integer;
	end;

procedure WriteList(st, cur: numptr);
begin
	cur := st;
	while cur <> nil do
	begin
		write(cur^.data, ' ');
		cur := cur^.next
	end;
	writeln
end;

var
	st, fin, cur: numptr;
begin
	{$I-}
	new(cur);
	st := cur;
	read(cur^.data);
	CheckInput;
	new(fin);
	cur^.next := fin;
	while not SeekEof do
	begin
		cur := fin;
		read(cur^.data);
		CheckInput;
		new(fin);
		cur^.next := fin
	end;
	cur^.next := nil;
	WriteList(st, cur);
	WriteList(st, cur)
end.
