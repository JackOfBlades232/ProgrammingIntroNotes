program HelloFile; { helofile.pas }
const
	filename = 'output_hello.txt';
	message = 'Hello, word!';
var
	f: text;
begin
	{$I-}
	assign(f, filename);
	rewrite(f);
	if IOResult <> 0 then
	begin
		writeln('Couldn''t open file ', filename);
		halt(1)
	end;
	writeln(f, message);
	if IOResult <> 0 then
	begin
		writeln('Couldn''t write to file ', filename);
		halt(1)
	end;
	close(f)
end.
