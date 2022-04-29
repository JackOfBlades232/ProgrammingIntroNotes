program recursion;

procedure PrintChars(ch: char; count: integer);
begin
	if count > 0 then
	begin
		write(ch);
		PrintChars(ch, count - 1)
	end
end;

procedure PrintDigitsOfNumber(number: integer);
begin
	if number > 0 then
	begin
		PrintDigitsOfNumber(number div 10);
		write(number mod 10, ' ');
	end
end;

function DoReverseNumber(n, m: longint): longint;
begin
	if n = 0 then
		DoReverseNumber := m
	else
		DoReverseNumber := DoReverseNumber(n div 10, m * 10 + n mod 10)
end;

function ReverseNumber(n: longint): longint;
begin
	ReverseNumber := DoReverseNumber(n, 0)
end;

var
	ch: char;
	n: integer;
begin
	write('Input char: ');
	readln(ch);
	write('Input integer: ');
	readln(n);
	write('12 chars: ');
	PrintChars(ch, 12);
	writeln;
	write('Digits of n: ');
	PrintDigitsOfNumber(n);
	writeln;
	writeln('Reversed n: ', ReverseNumber(n))
end.
