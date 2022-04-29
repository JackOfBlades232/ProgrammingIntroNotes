program LiteralArrays; { literal_arrays.pas }
var
	hello: array [1..30] of char;
begin
	hello := 'Hello, world!'; { not the smartest way, the array will hale 17 #0 chars at the end }
	writeln(hello)
end.
