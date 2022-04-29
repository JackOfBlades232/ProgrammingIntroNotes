program Problem2_11; { 2_11.pas }

function GetNumberOfOnes(n: longint): integer;
begin
    GetNumberOfOnes := 0;
    if n < 0 then
    begin
        GetNumberOfOnes := 1;
        n := -n
    end;
    while n <> 0 do
    begin
        if (n and 1) = 1 then
            GetNumberOfOnes := GetNumberOfOnes + 1;
        n := n shr 1
    end
end;

var
    n: longint;
begin
    readln(n);
    writeln(GetNumberOfOnes(n))
end.
