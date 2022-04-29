program RecordProg; { record.pas }
type
	CheckPoint := record
		n: integer;
		latitude, longtitude: real;
		hidden: boolean;
		penalty: integer;
	end;
var
	cp: CheckPoint;
begin
	cp.n := 70;
	cp.latitude := 54.83843;
	cp.longtitude := 37.59556;
	cp.hidden := false;
	cp.penalty := 30
end.
