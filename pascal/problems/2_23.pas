program Problem2_23; { 2_23.pas }

function CharIsNotSpace(c: char): boolean;
begin
    CharIsNotSpace := (c <> ' ') and (c <> #9) and (c <> #10) and (c <> #13)
end;

function CountWordsInString(s: string): integer;
var
    i: integer;
begin
    CountWordsInString := 0;
    for i := 1 to length(s) do
        if CharIsNotSpace(s[i]) and ((i = 1) or (not CharIsNotSpace(s[i - 1]))) then
            CountWordsInString := CountWordsInString + 1
end;

begin
    if ParamCount > 0 then
        writeln('Num words in first cl argument: ', CountWordsInString(ParamStr(1)))
end.
