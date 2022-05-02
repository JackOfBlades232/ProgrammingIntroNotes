program Problem2_31; { 2_31.pas }
uses crt;
const
    CentralSquareSize = 5;
	ColorCount = 16;
	BGColorCount = 8;
	InitColor = White;
	InitBGColor = Black;
type
    tag = record
        CurX, CurY, dx, dy: integer;
    end;

procedure GetKey(var code: integer);
var
    c: char;
begin
    c := ReadKey;
    if c = #0 then
    begin
        c := ReadKey;
        code := -ord(c)
    end
    else
    begin
        code := ord(c)
    end
end;

procedure DrawSquare(x, y: integer);
var
    i, j: integer;
begin
    for i := 1 to CentralSquareSize do
    begin
        GotoXY(x, y);
        for j := 1 to CentralSquareSize do
            write('*');
        y := y + 1
    end;
    GotoXY(1, 1)
end;

procedure ChangeTextColor(var color: word; increment: integer);
begin
    color := (color + increment) mod ColorCount;
    TextColor(color)
end;

procedure ChangeBGColor(var color: word; increment: integer);
begin
    color := (color + increment) mod BGColorCount;
    TextBackground(color)
end;

var
    SaveTextAttr, x, y, c: integer;
    color, BGColor: word;
begin
    SaveTextAttr := TextAttr;
    color := InitColor;
    BGColor := InitBGColor;
    TextColor(color);
    if (ScreenWidth < CentralSquareSize + 2) or
        (ScreenWidth < CentralSquareSize + 2) then
    begin
        writeln('Terminal window is too small');
        halt(1)
    end;
    x := (ScreenWidth - CentralSquareSize) div 2;
    y := (ScreenHeight - CentralSquareSize) div 2;
    clrscr;
    DrawSquare(x, y);
    while true do
    begin
        if KeyPressed then
        begin
            GetKey(c);
            case c of
                27: { escape }
                    break;
                32: { space }
                    break;
                97: { left arrow }
                    ChangeBGColor(BGColor, -1);
                100: { right arrow }
                    ChangeBGColor(BGColor, 1);
                115: { down arrow }
                    ChangeTextColor(color, -1);
                119: { up arrow }
                    ChangeTextColor(color, 1)
            end;
            DrawSquare(x, y)
        end
    end;
    TextAttr := SaveTextAttr;
    clrscr
end.
