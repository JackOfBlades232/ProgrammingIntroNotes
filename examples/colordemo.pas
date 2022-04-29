program ColorsDemo;
uses crt;

const
    ColorCount = 16;
    BGColCount = 8;
var
    AllColors: array [1..ColorCount] of word =
    (
        Black, Blue, Green, Cyan,
        Red, Magenta, Brown, LightGray,
        DarkGray, LightBlue, LightGreen, LightCyan,
        LightRed, LightMagenta, Yellow, White
    );

procedure MakeLine(line: integer; fgcolor: word);
var
    i, w, cw: integer;
begin
    w := ScreenWidth;
    cw := w div BGColCount;
    if cw = 0 then
        cw := 1;
    if line = ScreenHeight then
        w := w - 1;
    for i := 1 to w do
    begin
        GotoXY(i, line);
        TextBackground(AllColors[(i-1) div cw + 1]);
        if i mod 2 = 0 then
            TextColor(fgcolor + blink)
        else
            TextColor(fgcolor);
        write('*')
    end
end;

procedure MakeScreen;
var
    i: integer;
begin
    clrscr;
    for i := 1 to ScreenHeight do
        MakeLine(i, AllColors[i mod ColorCount + 1])
end;

begin
    MakeScreen;
    readln;
    write(#27'[0m');
    clrscr
end.
