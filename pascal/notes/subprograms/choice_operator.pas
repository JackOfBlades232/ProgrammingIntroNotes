program ChoiceOperator; { choice_operator.pas }
type
	AgeCategories = 
		(child, adolecent, youngling, adult, senior);

procedure HandleChild;
begin
	writeln('No worries')
end;

procedure HandleAdolecent;
begin
	writeln('Chill bruh')
end;

procedure HandleYoungling;
begin
	writeln('Start working')
end;

procedure HandleAdult;
begin
	writeln('Push push push')
end;

procedure HandleSenior;
begin
	writeln('You''ve earned it')
end;

function GetAgeCategory(age: integer): AgeCategories;
begin
	case age of
		0..12:
			GetAgeCategory := child;
		13..18:
			GetAgeCategory := adolecent;
		19..30:
			GetAgeCategory := youngling;
		31..60:
			GetAgeCategory := adult;
		61..1000:
			GetAgeCategory := senior;
		else
		begin
			writeln('Invalid age');
			halt(1)
		end
	end
end;	

var
	age: integer;
	category: AgeCategories;
begin
	readln(age);
	category := GetAgeCategory(age);
	case category of
		child:
			HandleChild;
		adolecent:
			HandleAdolecent;
		youngling:
			HandleYoungling;
		adult:
			HandleAdult;
		senior:
			HandleSenior
	end
end.
