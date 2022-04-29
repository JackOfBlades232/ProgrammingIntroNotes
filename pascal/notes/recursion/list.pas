program List; { list.pas }
type
    itemptr = ^item;
    item = record
        data: integer;
        next: itemptr;
    end;

function ItemListSum(p: itemptr): integer; { simpler through recursion }
begin
    if p = nil then
        ItemListSum := 0
    else
        ItemListSum := p^.data + ItemListSum(p^.next)
end;

procedure DisposeItemList(p: itemptr);
begin
    if p = nil then
        exit;
    DisposeItemList(p^.next);
    dispose(p)
end;

procedure AddNumIntoSortedList(var p: itemptr; n: integer);
var
    tmp: itemptr;
begin
    if (p = nil) or (p^.data > n) then
    begin
        new(tmp);
        tmp^.data := n;
        tmp^.next := p;
        p := tmp
    end
    else
        AddNumIntoSortedList(p^.next, n)
end;

var
	first, tmp: itemptr;
	n: integer;
begin
	first := nil; 
	while not SeekEoln do
	begin
		read(n);
		AddNumIntoSortedList(first, n)
	end;
    writeln('Sum of list: ', ItemListSum(first));
    DisposeItemList(first);
    writeln('Sum of list after disposal: ', ItemListSum(first), ' (random val in memory)')
end.
