program MyStringTools; { my_string_tools.pas }
const
	ToNumOffset = -ord('0');
	ToLowerCaseOffset = ord('a') - ord('A');
	ToUpCaseOffset = -ToLowerCaseOffset;
	digit = 10;

function ShiftCharsInString(s: string; min, max: char; offset: integer): string;
var
	answer: string;
	i: integer;
	c: char;
begin
	answer := '';
	for i := 1 to length(s) do
	begin
		c := s[i];
		if (c >= min) and (c <= max) then
			c := chr(ord(c) + offset);
		answer := answer + c
	end;
	ShiftCharsInString := answer
end; 

function MyLowerCase(s: string): string;
begin
	MyLowerCase := ShiftCharsInString(s, 'A', 'Z', ToLowerCaseOffset) 
end; 

function MyUpCase(s: string): string;
begin
	MyUpCase := ShiftCharsInString(s, 'a', 'z', ToUpCaseOffset) 
end; 

procedure CheckSubstringChoice(s: string; start, len: integer);
begin
	if len < 0 then
	begin
		writeln('Invalid length: ', len);
		halt(1)
	end;
	if (start > length(s)) or (start < 1) then
	begin
		writeln('Start position: ', start, ' is greater than length: ', length(s));
		halt(1)
	end
end;

function MyCopy(s: string; start, len: integer): string;
var
	answer: string;
	i, finish: integer;
begin
	CheckSubstringChoice(s, start, len);
	answer := '';
	if length(s) - start - 1 < len then
		finish := length(s)
	else
		finish := start + len - 1;
	for i := start to finish do
		answer := answer + s[i];
	MyCopy := answer
end;

procedure MyDelete(var s: string; start, len: integer);
var
	half1, half2: string;
	i, finish: integer;
begin
	CheckSubstringChoice(s, start, len);
	half1 := '';
	half2 := '';
	if length(s) - start - 1 < len then
		finish := length(s)
	else
		finish := start + len - 1;
	for i := 1 to start - 1 do
		half1 := half1 + s[i];
	for i := finish + 1 to length(s) do
		half2 := half2 + s[i];
	s := half1 + half2
end;

procedure MyInsert(child: string; var parent: string; start: integer);
var
	half1, half2: string;
	i: integer;
begin
	CheckSubstringChoice(parent, start, 0);
	half1 := '';
	half2 := '';
	for i := 1 to start - 1 do
		half1 := half1 + parent[i];
	for i := start to length(parent) do
		half2 := half2 + parent[i];
	parent := half1 + child + half2
end;

function MyPos(obj, parent: string): integer;
var
	i, position: integer;
begin
	position := 1;
	for i := 1 to length(parent) do
	begin
		if parent[i] = obj[position] then
			position := position + 1
		else
			position := 1;
		if position = length(obj) + 1 then
		begin
			MyPos := i - length(obj) + 1;
			exit
		end
	end;
	MyPos := 0
end;

function CharValidForInteger(c: char; index: integer): boolean;
begin
	if (c > '9') or ((c < '0') and (c <> '-')) then
	begin
		CharValidForInteger := false;
		exit
	end;
	if (c = '-') and (index > 1) then
	begin
		CharValidForInteger := false;
		exit
	end;
	CharValidForInteger := true
end; 

procedure MyVal(s: string; var out, code: integer);
var
	c: char;
	i, modifier, answer: integer;
begin
	answer := 0;
	modifier := 1;
	for i := length(s) downto 1 do
	begin
		c := s[i];
		if CharValidForInteger(c, i) then
		begin
			if c = '-' then
				answer := -answer
			else
			begin
				answer := answer + modifier * (ord(c) + ToNumOffset);
				modifier := modifier * 10
			end
		end
		else
		begin
			code := i;
			exit
		end	 
	end;
	out := answer;
	code := 0
end; 

function MyStr(n: integer): string;
var
	s, appendix: string;
	num: integer;
begin
	if n = 0 then
		MyStr := '0';
	if n > 0 then
		appendix := ''
	else
	begin
		appendix := '-';
		n := -n
	end;
	s := '';
	while n > 0 do
	begin
		num := n mod digit;
		n := n div digit;
		s := chr(num - ToNumOffset) + s
	end;
	MyStr := appendix + s
end; 

begin 
	writeln('All debugged, but not refactored')	
end.
