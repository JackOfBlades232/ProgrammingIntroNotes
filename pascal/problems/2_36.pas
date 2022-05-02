program Problem2_36; { 2_36.pas }
uses crt;
const
    DelayDuration = 100; { 0,1s }
    MovesInPhase = 10;
    Probability = 0.1;
type
    direction = (none, left, right, up, down);
    star = record
        CurX, CurY, DirectionIndex: integer;
    end;
var
    AllDirections: array [1..4] of direction = (
        left, up, right, down
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

procedure SetStarDirection(var s: star; dir: direction);
begin
    case dir of
        left:
            s.DirectionIndex := 1;
        up:
            s.DirectionIndex := 2;
        right:
            s.DirectionIndex := 3;
        down:
            s.DirectionIndex := 4
    end
end;

procedure DecodeRandomAndChangeDirection(var s: star; rand: longint);
begin
    if (rand <> 1) and (rand <> 3) then
    begin
        writeln(ErrOutput, 'Incorrect random direction code: ', rand,
            ', must be from 1 or 3');
        clrscr;
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
    InputDir: direction;
    ticker, c: integer;
begin
    randomize;
    clrscr;
    InitStar(s);
    DrawStar(s);
    ticker := MovesInPhase;
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
                97: { left }
                    InputDir := left;   
                100: { right }
                    InputDir := right;   
                119: { up }
                    InputDir := up;   
                115: { down }
                    InputDir := down;   
            end
        end
        else
            InputDir := none;
        if (ticker >= MovesInPhase) and (InputDir <> none) then
        begin
            SetStarDirection(s, InputDir);
            ticker := 0
        end
        else if random < Probability then
            DecodeRandomAndChangeDirection(s, 1 + random(2) * 2);
        delay(DelayDuration);
        MoveStar(s);
        ticker := ticker + 1
    end;
    clrscr
end.
