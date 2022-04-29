program Problem2_18; { 2_18.pas }

procedure ReadChar(c: char; rank: integer; var success: boolean; var res: longint);
var
    temp: longint;
begin
    temp := -1;
    if (c >= 'A') and (c <= 'Z') then
        temp := ord(c) - ord('A') + 10;
    if (c >= 'a') and (c <= 'z') then
        temp := ord(c) - ord('a') + 10;
    if (c >= '0') and (c <= '9') then
        temp := ord(c) - ord('0');
    if temp = -1 then
    begin
        writeln('Trying to read a char, but this is invalid: ', c);
        success := false;
        exit
    end;
    if temp >= rank then
    begin
        writeln('This char ''', c, ''' represents too big of a number: ', temp);
        success := false;
        exit
    end;
    success := true;
    res := temp
end;

procedure ReadLongint(rank: integer; var success: boolean; var res: longint);
var
	c: char;
	number, digit: longint;
	position: integer;
begin
	number := 0;
	position := 0;
    digit := 0;
	repeat
		read(c);
		position := position + 1;
	until (c <> ' ') and (c <> #10);
	while (c <> ' ') and (c <> #10) do
	begin
		ReadChar(c, rank, success, digit);
        if not success then
        begin
			readln;
			exit
        end;
		number := number * rank + digit;
		read(c);
		position := position + 1;
	end;
	res := number;
	success := true
end;

var
    x: longint;
    rank: integer;
    success: boolean;
begin
    write('Input rank: ');
    readln(rank);
    if (rank < 2) or (rank > 36) then
    begin
        write('Invalid rank, input again: ');
        readln(rank)
    end;
    repeat
        ReadLongint(rank, success, x);
    until success;
    writeln('The number is ', x)
end.
