program BasicPointers; { basic_pointers.pas }
var
	r: real;
	p: ^real;
	ap: pointer; { untyped pointer, takes the type of the first address-assigned variable, better not to use }
begin
	{$T+}
	r := 10.0;
	p := @r; { @ takes an untyped address, but if $T- is active then it is typed }
	writeln('r: ', r, ' val in p: ', p^);
	p^ := 25.7;
	writeln('r: ', r, ' val in p: ', p^);
	p := nil { null address (untyped), indicates that this pointer points to nothing }
end.
