program MovingStar;
uses crt;
const
    DelayDuration = 100;

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

type
    star = record
        CurX, CurY, dx, dy: integer;
    end;

procedure ShowStar(var s: star);
begin
    GotoXY(s.CurX, s.CurY);
    write('*');
    GotoXY(1, 1)
end;

procedure HideStar(var s: star);
begin
    GotoXY(s.CurX, s.CurY);
    write(' ');
    GotoXY(1, 1)
end;

procedure MoveStar(var s: star);
begin
    HideStar(s);
    s.CurX := s.CurX + s.dx;
    if s.CurX > ScreenWidth then
        s.CurX := 1
    else
    if s.CurX < 1 then
        s.CurX := ScreenWidth;
    s.CurY := s.CurY + s.dy;
    if s.CurY > ScreenHeight then
        s.CurY := 1
    else
    if s.CurY < 1 then
        s.CurY := ScreenHeight;
    ShowStar(s)
end;

procedure SetDirection(var s: star; dx, dy: integer);
begin
    s.dx := dx;
    s.dy := dy
end;

var
    s: star;
    c: integer;
begin
    clrscr;
    s.CurX := ScreenWidth div 2;
    s.CurY := ScreenHeight div 2;
    s.dx := 0;
    s.dy := 0;
    ShowStar(s);
    while true do
    begin
        if not KeyPressed then
        begin
            MoveStar(s);
            delay(DelayDuration);
            continue
        end;
        GetKey(c);
        case c of
            -75: SetDirection(s, -1, 0);
            -77: SetDirection(s, 1, 0);
            -72: SetDirection(s, 0, -1);
            -80: SetDirection(s, 0, 1);
            32:  SetDirection(s, 0, 0);
            27:  break
        end
    end;
    clrscr
end.
