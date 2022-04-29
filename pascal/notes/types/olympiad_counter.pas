program OlympiadCounter; { olympiad_counter.pas }
const
	MaxSchool = 67;
	MaxGroup = 100;
type
	StudentCounts = array [1..MaxSchool] of integer;
var
	schools: StudentCounts; 
	i, number, MaxIndex: integer;
begin
	for i := 1 to MaxSchool do
		schools[i] := 0;
	{$I-}
	while not SeekEof do
	begin
		read(number);
		if IOResult <> 0 then
		begin
			writeln('Incorrect data format');
			halt(1)
		end;
		i := number div MaxGroup;
		if (i < 1) or (i > MaxSchool) then
		begin
			writeln('Illegal school id: ', i, ' [', number, '] ');
			halt(1)
		end;
		schools[i] := schools[i] + 1;
		readln
	end;
	MaxIndex := 0;
	for i := 1 to MaxSchool do
	begin
		if schools[i] > schools[MaxIndex] then
		begin
			MaxIndex := i;
		end
	end;
	write('Most students in schools: ');
	for i := 1 to MaxSchool do
		if (schools[i] = schools[MaxIndex]) then
			write(i, ' ');
	writeln;
end. 
