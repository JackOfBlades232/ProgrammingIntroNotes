program Problem2_29; { 2_29.pas }
uses crt;
const
    CentralSquareSize = 10;
    DelayDuration = 100; { ms }
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

procedure HandleTurn(var t: tag; MinX, MaxX, MinY, MaxY: integer);
begin
    if ((t.dx < 0) and (t.CurX <= MinX)) or
        ((t.dx > 0) and (t.CurX >= MaxX)) then
    begin
        t.dx := 0;
        if t.CurY >= ScreenHeight div 2 then
            t.dy := -1
        else
            t.dy := 1
    end
    else if ((t.dy < 0) and (t.CurY <= MinY)) or
        ((t.dy > 0) and (t.CurY >= MaxY)) then
    begin
        t.dy := 0;
        if t.CurX >= ScreenWidth div 2 then
            t.dx := -1
        else
            t.dx := 1
    end;
end;

procedure ChangeDirection(var t: tag);
begin
    t.dx := -t.dx;
    t.dy := -t.dy
end;

var
    t: tag;
    x, y, c: integer;
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
    t.dx := 1;
    t.dy := 0;
    while true do
    begin
        if KeyPressed then
        begin
            GetKey(c);
            if c = 32 then { space }
                ChangeDirection(t)
            else if c = 27 then { esc }
                break
        end;
        delay(DelayDuration);
        HandleTurn(t, x, x + CentralSquareSize - 1,
            y, y + CentralSquareSize - 1);
        MoveTag(t)
    end;
    clrscr
end.
