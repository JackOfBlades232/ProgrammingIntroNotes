program DynamicVariables; { dynamic_variables.pas }
var
	p, q: ^string;
begin
	new(p); { create new dynamic var in the address specified in the pointer }
   	{ calling new more than once without deleting the var leads to a memory leak (the first var stays in memory, but is inaccessible }
	p^ := 'This is a string, which resides in the heap';
	q := p; { more than one ptr can point to a dynamic var }
	dispose(q) { deletes the dynamic variable and frees memory in the heap (but does not reduce the heap capacity) }
end.
