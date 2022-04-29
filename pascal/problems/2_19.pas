program Problem2_19; { 2_19.pas }

function CharIsNotSpace(c: char): boolean;
begin
    CharIsNotSpace := (c <> ' ') and (c <> #9) and (c <> #10) and (c <> #13)
end;

procedure ReadWord(var NumLetters, NumSpaces: integer; 
                   var FirstLetter, LastLetter: char);
var
    c, PrevC: char;
begin
    NumLetters := 0;
    NumSpaces := 0;
    repeat
    begin
        read(c);
        NumSpaces := NumSpaces + 1
    end
    until CharIsNotSpace(c);
    while CharIsNotSpace(c) do
    begin
        PrevC := c;
        read(c);
        NumLetters := NumLetters + 1
    end;
    FirstLetter := c;
    LastLetter := PrevC
end;

procedure CountWords;
var
    FirstLetter, LastLetter: char;
    counter, NumLetters, NumSpaces: integer;
begin
    counter := 0;
    while not eof do
    begin
        ReadWord(NumLetters, NumSpaces, FirstLetter, LastLetter);
        counter := counter + 1
    end;
    writeln('Num words: ', counter)
end;

procedure CountOddAndEven;
var
    FirstLetter, LastLetter: char;
    OddCounter, EvenCounter, NumLetters, NumSpaces: integer;
begin
    OddCounter := 0;
    EvenCounter := 0;
    while not eof do
    begin
        ReadWord(NumLetters, NumSpaces, FirstLetter, LastLetter);
        if NumLetters mod 2 = 0 then
            EvenCounter := EvenCounter + 1
        else
            OddCounter := OddCounter + 1
    end;
    writeln('Num odd words: ', OddCounter);
    writeln('Num even words: ', EvenCounter)
end;

procedure CountSevenTwo;
var 
    FirstLetter, LastLetter: char;
    SevenCounter, TwoCounter, NumLetters, NumSpaces: integer;
begin
    SevenCounter := 0;
    TwoCounter := 0;
    while not eof do
    begin
        ReadWord(NumLetters, NumSpaces, FirstLetter, LastLetter);
        if NumLetters > 7 then
            SevenCounter := SevenCounter + 1;
        if NumLetters <= 2 then
            TwoCounter := TwoCounter + 1
    end;
    writeln('Num of longer-than-7 words: ', SevenCounter);
    writeln('Num of not-longer-than-2 words: ', TwoCounter)
end;

procedure CountAZ;
var 
    FirstLetter, LastLetter: char;
    ACounter, ZCounter, NumLetters, NumSpaces: integer;
begin
    ACounter := 0;
    ZCounter := 0;
    while not eof do
    begin
        ReadWord(NumLetters, NumSpaces, FirstLetter, LastLetter);
        if FirstLetter = 'A' then
            ACounter := ACounter + 1;
        if LastLetter = 'z' then
            ZCounter := ZCounter + 1
    end;
    writeln('Num of A-starting words: ', ACounter);
    writeln('Num of z-ending words: ', ZCounter)
end;

procedure CountMinMaxWord;
var 
    FirstLetter, LastLetter: char;
    counter, MinLength, MaxLength, NumLetters, NumSpaces: integer;
begin
    counter := 0;
    while not eof do
    begin
        ReadWord(NumLetters, NumSpaces, FirstLetter, LastLetter);
        counter := counter + 1;
        if (NumLetters < MinLength) or (counter = 0) then
            MinLength := NumLetters;
        if (NumLetters > MaxLength) or (counter = 0) then
            MaxLength := NumLetters
    end;
    writeln('Num words: ', counter);
    writeln('Longest word: ', MaxLength);
    writeln('Shortest word: ', MinLength)
end;

procedure CountMaxWordMaxSpace;
var 
    FirstLetter, LastLetter: char;
    counter, MaxLength, MaxSpace, NumLetters, NumSpaces: integer;
begin
    counter := 0;
    while not eof do
    begin
        ReadWord(NumLetters, NumSpaces, FirstLetter, LastLetter);
        counter := counter + 1;
        if (NumSpaces > MaxSpace) or (counter = 0) then
            MaxSpace := NumSpaces;
        if (NumLetters > MaxLength) or (counter = 0) then
            MaxLength := NumLetters
    end;
    writeln('Longest word: ', MaxLength);
    writeln('Longest space string: ', MaxSpace)
end;

procedure VerifyBracketBalance;
var
    c: char;
    indicator: integer;
begin
    indicator := 0;
    while not eof do
    begin
        read(c);
        if c = '(' then
            indicator := indicator + 1;
        if c = ')' then
            indicator := indicator - 1
    end;
    if indicator = 0 then
        writeln('Bracket balance: YES')
    else
        writeln('Bracket balance: NO')
end;

procedure CountBracketPairs;
var
    c: char;
    indicator, counter: integer;
begin
    counter := 0;
    indicator := 0;
    while not eof do
    begin
        read(c);
        if indicator = 1 then
        begin
            if c = ')' then
                counter := counter + 1;
            indicator := 0
        end;
        if c = '(' then
            indicator := 1
    end;
    writeln('Bracket pairs: ', counter)
end;

begin
    CountMaxWordMaxSpace
end.
