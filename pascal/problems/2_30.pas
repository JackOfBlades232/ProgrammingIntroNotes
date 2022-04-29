program Problem2_30; { 2_30.pas }
uses crt;
const
    MinSize = 1;
    MinLeftover = 1;
    InitSize = 3;
type
    DrawZone = record
        XMin, XMax, YMin, YMax: integer;
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

procedure InitZone(var z: DrawZone);
begin
    z.XMin := (ScreenWidth - InitSize) div 2 + 1;
    z.XMax := (ScreenWidth + InitSize) div 2;
    z.YMin := (ScreenHeight - InitSize) div 2 + 1;
    z.YMax := (ScreenHeight + InitSize) div 2
end;

procedure DrawHorizontalLine(c: char; y, XMin, XMax: integer);
var
    i: integer;
begin
    GotoXY(XMin, y);
    for i := 0 to XMax - XMin do
        write(c);
    GotoXY(1, 1)
end;

procedure DrawVerticalLine(c: char; x, YMin, YMax: integer);
var
    i: integer;
begin
    for i := YMin to YMax do
    begin
        GotoXY(x, i);
        write(c)
    end;
    GotoXY(1, 1)
end;

procedure ShowZone(var z: DrawZone);
var
    y: integer;
begin
    for y := z.YMin to z.YMax do
        DrawHorizontalLine('*', y, z.XMin, z.XMax)
end;

procedure ExpandZoneHorizontal(var z: DrawZone; MaxX: integer);
begin
    if (z.XMin > MinLeftover + 1) and (z.XMax < MaxX) then
    begin
        z.XMin := z.XMin - 1;
        z.XMax := z.XMax + 1;
        DrawVerticalLine('*', z.XMin, z.YMin, z.YMax);
        DrawVerticalLine('*', z.XMax, z.YMin, z.YMax)
    end;
end;

procedure ExpandZoneVertical(var z: DrawZone; MaxY: integer);
begin
    if (z.YMin > MinLeftover + 1) and (z.YMax < MaxY) then
    begin
        z.YMin := z.YMin - 1;
        z.YMax := z.YMax + 1;
        DrawHorizontalLine('*', z.YMin, z.XMin, z.XMax);
        DrawHorizontalLine('*', z.YMax, z.XMin, z.XMax)
    end;
end;

procedure ShrinkZoneHorizontal(var z: DrawZone);
begin
    if z.XMax - z.XMin > MinSize - 1 then
    begin
        DrawVerticalLine(' ', z.XMin, z.YMin, z.YMax);
        DrawVerticalLine(' ', z.XMax, z.YMin, z.YMax);
        z.XMin := z.XMin + 1;
        z.XMax := z.XMax - 1
    end;
end;

procedure ShrinkZoneVertical(var z: DrawZone);
begin
    if z.YMax - z.YMin > MinSize - 1 then
    begin
        DrawHorizontalLine(' ', z.YMin, z.XMin, z.XMax);
        DrawHorizontalLine(' ', z.YMax, z.XMin, z.XMax);
        z.YMin := z.YMin + 1;
        z.YMax := z.YMax - 1
    end;
end;

var
    z: DrawZone;
    MaxX, MaxY, c: integer;
begin
    MaxX := ScreenWidth - MinLeftover;
    MaxY := ScreenHeight - MinLeftover;
    clrscr;
    InitZone(z);
    ShowZone(z);
    while true do
    begin
        if KeyPressed then
        begin
            GetKey(c);
            case c of
                97: { left }
                    ShrinkZoneHorizontal(z);
                100: { right }
                    ExpandZoneHorizontal(z, MaxX);
                119: { up }
                    ExpandZoneVertical(z, MaxY);
                115: { down }
                    ShrinkZoneVertical(z);
                32: { space }
                    break;
                27: { escape }
                    break
            end
        end
    end;
    clrscr
end.
