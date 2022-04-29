program string_tools; { string_tools.pas }
var
	s: string;
	n: longint;
	code: word;
begin
	s := 'abrakadabra';
	writeln(length(s));
	SetLength(s, 4); { leaves allocated memory unchanged }
	writeln(LowerCase(s));
	writeln(UpCase(s));
	writeln(copy(s, 3, 5));
	delete(s, 3, 2);
	writeln(s);
	insert('PWR', s, 3);
	writeln(s);
	writeln(pos('bra', s));
	val(s, n, code);
	writeln(n, ' ', code);
	str(12.5, s);
	writeln(s)
end.	
