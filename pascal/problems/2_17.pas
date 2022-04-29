program Problem2_17; { 2_17.pas }
var
	i, j: integer;
	c: char;
begin
	write('  |');
	for c := '4' to '9' do
		write(' .', c);
	for c := 'A' to 'F' do
		write(' .', c);
	writeln;
	write('  |');
	for i := 4 to 15 do
		write('---');

	writeln;
	for i := 0 to 7 do
	begin
		write(i, '.|');
		for j := 4 to 15 do
			write('  ', chr(j * 8 + i));
		writeln
	end
end.
