program InsertOrRename; { insert_or_rename.pas }
var
	f: text;
begin
	{$I-}
	if ParamCount < 2 then
	begin
		writeln(ErrOutput, 'Please specify operation and filename');
		halt(1)
	end;
	assign(f, ParamStr(2));
	if (ParamStr(1) <> 'erase') and (ParamStr(1) <> 'rename') then
	begin
		writeln('Please specify valid operation');
		halt(1)
	end;
	if ParamStr(1) = 'erase' then
		erase(f);
	if ParamStr(1) = 'rename' then
	begin
		if ParamCount < 3 then
		begin
			writeln(ErrOutput, 'Please specify new name');
			halt(1)
		end;
		rename(f, ParamStr(3))
	end;
	if IOResult <> 0 then
	begin
		writeln(ErrOutput, 'Error applying operaion to the file');
		halt(1)
	end
end.
