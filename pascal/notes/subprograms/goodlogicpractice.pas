program goodlogicpractice;

function IsLatinLetter(ch: char): boolean;
begin
	IsLatinLetter := 
		((ch >= 'A') and (ch <= 'Z')) or
		((ch >= 'a') and (ch <= 'z'))
end;

var
	ch: char;
begin
	write('Input char: ');
	readln(ch);
	if IsLatinLetter(ch) then
		writeln(ch, ' is a latin letter')
	else
		writeln(ch, ' is not a latin letter')
end.
