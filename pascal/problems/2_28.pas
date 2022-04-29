program Problem2_28; { 2_28.pas }
uses crt;
const
    CentralSquareSize = 10;
    DelayDuration = 100; { ms }
type
    tag = record
        CurX, CurY, dx, dy: integer;
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

procedure DrawTag(var t: tag);
begin
    GotoXY(t.CurX, t.CurY);
    write('#');
    GotoXY(1, 1)
end;

procedure HideTag(var t: tag; EmptySymbol: char);
begin
    GotoXY(t.CurX, t.CurY);
    write(EmptySymbol);
    GotoXY(1, 1)
end;

procedure MoveTag(var t: tag);
begin
    HideTag(t, '*');
    t.CurX := t.CurX + t.dx;
    t.CurY := t.CurY + t.dy;
    DrawTag(t)
end;

procedure RepositionTag(var t: tag; x, y: integer);
begin
    HideTag(t, ' ');
    t.CurX := x;
    t.CurY := y;
    DrawTag(t)
end;

procedure CoverLine(var t: tag; len, dx, dy: integer);
var
    i: integer;
begin
    t.dx := dx;
    t.dy := dy;
    for i := 2 to len do
    begin
        delay(DelayDuration);
        if KeyPressed then
        begin
            clrscr;
            halt(0)
        end;
        MoveTag(t)
    end
end;

var
    t: tag;
    x, y: integer;
begin
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
    RepositionTag(t, x, y);
    while not KeyPressed do
    begin
        CoverLine(t, CentralSquareSize, 1, 0);
        CoverLine(t, CentralSquareSize, 0, 1);
        CoverLine(t, CentralSquareSize, -1, 0);
        CoverLine(t, CentralSquareSize, 0, -1)
    end;
    clrscr
end.
