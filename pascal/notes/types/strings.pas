program strings; { strings.pas }
var
	s1: string; { max len -- 255 }
	s2: string[15]; { max len 15 }
begin
	s1 := 'first'#10#9'second'#10#9'third'#10#9'fourth';
	writeln(s1);
	writeln('first elem of s1 = ', s1[1]);
	writeln('len of s1 = ', ord(s1[0]), ' = ', length(s1));
	s2 := s1;
	writeln(s2);
	s1 := s1 + s2;
	writeln(s1)
end. 
