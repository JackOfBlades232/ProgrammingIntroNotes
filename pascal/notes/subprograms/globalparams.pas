program globalparams;
var
	gl: integer;

procedure ChangeGl;
begin
	gl := 3;
end;

var
	loc: integer;
begin
	ChangeGl;
	writeln('gl (', gl, ') is global and can be changed in subprograms'); 
	loc := 4;
	writeln('loc (', loc, ') is local for the main part of the program and can not be changed in subprograms') 
end.
