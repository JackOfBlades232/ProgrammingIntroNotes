program Problem2_33; { 2_33.pas }
uses crt;
const
    DelayDuration = 100; { 0,1s }
    Probability = 0.1;
type
    direction = (left, right, up, down);
    star = record
        CurX, CurY, DirectionIndex: integer;
    end;
var
    AllDirections: array [1..4] of direction = (
        left, right, up, down
    );

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

procedure HideStar(var s: star);
begin
    GotoXY(s.CurX, s.CurY);
    write(' ');
    GotoXY(1, 1)
end;

procedure DrawStar(var s: star);
begin
    GotoXY(s.CurX, s.CurY);
    write('*');
    GotoXY(1, 1)
end;

procedure WrapStar(var s: star);
begin
    if s.CurX < 1 then
        s.CurX := ScreenWidth
    else if s.CurX > ScreenWidth then
        s.CurX := 1;
    if s.CurY < 1 then
        s.CurY := ScreenHeight
    else if s.CurY > ScreenHeight then
        s.CurY := 1
end;

procedure MoveStar(var s: star);
begin
    HideStar(s);
    case AllDirections[s.DirectionIndex] of
        left:
            s.CurX := s.CurX - 1;
        right:
            s.CurX := s.CurX + 1;
        up:
            s.CurY := s.Cury - 1;
        down:
            s.CurY := s.CurY + 1
    end;
    WrapStar(s);
    DrawStar(s)
end;

procedure DecodeRandomAndChangeDirection(var s: star; rand: longint);
begin
    if (rand < 1) or (rand > 3) then
    begin
        writeln(ErrOutput, 'Incorrect random direction code: ', rand,
            ', must be from 1 to 3');
        halt(1)
    end;
    s.DirectionIndex := ((s.DirectionIndex + rand - 1) mod 4) + 1
end;

procedure InitStar(var s: star);
begin
    s.CurX := ScreenWidth div 2;
    s.CurY := ScreenHeight div 2;
    s.DirectionIndex := random(4) + 1
end;

var
    s: star;
    c: integer;
    rand: real;
begin
    randomize;
    clrscr;
    InitStar(s);
    DrawStar(s);
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
            end
        end;
        rand := random;
        if rand < Probability then
            DecodeRandomAndChangeDirection(s, random(3) + 1);
        delay(DelayDuration);
        MoveStar(s)
    end;
    clrscr
end.
