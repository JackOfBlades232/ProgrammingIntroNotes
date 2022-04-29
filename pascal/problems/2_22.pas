program Problem2_22; { 2_22.pas }
type
    Dictionary = array [1..255] of integer;

procedure PrintLongestArgument;
var
    i, MaxLength: integer;
    MaxLengthName: string;
begin
    MaxLength := 0;
    for i := 1 to ParamCount do
        if length(ParamStr(i)) > MaxLength then
        begin
            MaxLength := length(ParamStr(i));
            MaxLengthName := ParamStr(i)
        end;
    writeln('Longest word (', MaxLength, ' characters long): ', MaxLengthName);
end;

function HasNoRepeatingLetters(s: string): boolean;
var
    i: integer;
    CharDict: Dictionary;
begin
    for i := 1 to 255 do
        CharDict[i] := 0;
    for i := 1 to length(s) do
        CharDict[ord(s[i])] := CharDict[ord(s[i])] + 1;
    for i := 1 to 255 do
        if CharDict[i] > 1 then
        begin
            HasNoRepeatingLetters := false;
            exit
        end;
    HasNoRepeatingLetters := true
end;

procedure PrintNonRepeatingWords;
var
    i: integer;
begin
    write('Words without repeating letters: ');
    for i := 1 to ParamCount do
        if HasNoRepeatingLetters(ParamStr(i)) then
            write(ParamStr(i), ' ');
    writeln
end;

function HasDogNDot(s: string): boolean;
var
    i: integer;
    CharDict: Dictionary;
begin
    for i := 1 to 255 do
        CharDict[i] := 0;
    for i := 1 to length(s) do
        CharDict[ord(s[i])] := CharDict[ord(s[i])] + 1;
    HasDogNDot := (CharDict[64] = 1) and (CharDict[46] >= 1)
end;

procedure PrintDogNDot;
var
    i: integer;
begin
    write('Words with one @ and at least one dot: ');
    for i := 1 to ParamCount do
        if HasDogNDot(ParamStr(i)) then
            write(ParamStr(i), ' ');
    writeln
end;

function HasOnlyNumbers(s: string): boolean;
var
    i: integer;
begin
    for i := 1 to length(s) do
        if (s[i] < '0') or (s[i] > '9') then
        begin
            HasOnlyNumbers := false;
            exit
        end;
    HasOnlyNumbers := true
end;

procedure PrintOnlyNumbers;
var
    i: integer;
begin
    write('Words with only numbers: ');
    for i := 1 to ParamCount do
        if HasOnlyNumbers(ParamStr(i)) then
            write(ParamStr(i), ' ');
    writeln
end;

function HasOneUniqueChar(s: string): boolean;
var
    i: integer;
    FirstChar: char;
begin
    for i := 1 to length(s) do
    begin
        if i = 1 then
            FirstChar := s[i];
        if s[i] <> FirstChar then
        begin
            HasOneUniqueChar := false;
            exit
        end
    end;
    HasOneUniqueChar := true
end;

procedure PrintOneUniqueChar;
var
    i: integer;
begin
    write('Words with only one unique letter: ');
    for i := 1 to ParamCount do
        if HasOneUniqueChar(ParamStr(i)) then
            write(ParamStr(i), ' ');
    writeln
end;

function IsLatinLetter(c: char): boolean;
begin
    IsLatinLetter := ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z'))
end;

function HasLatinLetters(s: string): boolean;
var
    i: integer;
begin
    for i := 1 to length(s) do
        if IsLatinLetter(s[i]) then
        begin
            HasLatinLetters := true;
            exit
        end;
    HasLatinLetters := false
end;

procedure PrintWithLatinLetters;
var
    i: integer;
begin
    write('Words with at least one latin letter: ');
    for i := 1 to ParamCount do
        if HasLatinLetters(ParamStr(i)) then
            write(ParamStr(i), ' ');
    writeln
end;

function HasLettersInCommon(s: string; var CharDict: Dictionary): boolean;
var
    i: integer;
begin
    for i := 1 to length(s) do
        if CharDict[ord(s[i])] > 0 then
        begin
            HasLettersInCommon := true;
            exit
        end;
    HasLettersInCommon := false
end;

procedure PrintHasLettersInCommonWithFirst;
var
    i: integer;
    CharDict: Dictionary;
begin
    if ParamCount < 1 then
        exit;
    for i := 1 to 255 do
        CharDict[i] := 0;
    for i := 1 to length(ParamStr(1)) do
        CharDict[ord(ParamStr(1)[i])] := CharDict[ord(ParamStr(1)[i])] + 1;
    write('Words with at least one letter in common with the first one: ');
    for i := 2 to ParamCount do
        if HasLettersInCommon(ParamStr(i), CharDict) then
            write(ParamStr(i), ' ');
    writeln
end;


begin
    PrintLongestArgument;
    PrintNonRepeatingWords;
    PrintDogNDot;
    PrintOnlyNumbers;
    PrintOneUniqueChar;
    PrintWithLatinLetters;
    PrintHasLettersInCommonWithFirst
end.
