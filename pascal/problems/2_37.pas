program Problem2_36; { 2_36.pas }
uses crt, Unix;
const
    DelayDuration = 100; { 0,1s }
    FinishDelayDuration = 1000; { 1s }
    MovesInPhase = 10;
    Probability = 0.1;
    MaxMovements = 1000;
    MaxControlMotions = 50;
    WinZoneSize = 3;
    WinMessage = 'YOU WON';
    LoseMessage = 'GAME OVER';
type
    GameState = (finished, playing);
    direction = (none, left, right, up, down);
    star = record
        CurX, CurY, DirectionIndex: integer;
    end;
var
    AllDirections: array [1..4] of direction = (
        left, up, right, down
    );
    state: GameState;
    movements, ControlMotions: integer;

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

procedure HandleFinish(message: string);
begin
    if state = finished then
        exit;
    state := finished;
    clrscr;
    GotoXY((ScreenWidth - length(message)) div 2, ScreenHeight div 2);
    write(message);
    GotoXY(1, 1);
    delay(FinishDelayDuration)
end;

procedure ShowScore;
begin
    GotoXY(1, 1);
    write(MaxControlMotions - ControlMotions, ' ');
    write(MaxMovements - movements, ' ');
    GotoXY(1, 1)
end;

procedure HideStar(var s: star);
begin
    GotoXY(s.CurX, s.CurY);
    write(' ');
    GotoXY(1, 1)
end;

procedure DrawWinZone;
var
    i, j, x, y: integer;
begin
    x := (ScreenWidth - WinZoneSize) div 2;
    y := (ScreenHeight - WinZoneSize) div 2;
    for i := 1 to 3 do
    begin
        GotoXY(x, y);
        for j := 1 to 3 do
            write('@');
        y := y + 1
    end;
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
    if s.CurY <= 1 then
        s.CurY := ScreenHeight
    else if s.CurY > ScreenHeight then
        s.CurY := 2
end;

procedure CheckIfWon(var s: star);
var
    MinX, MinY, MaxX, MaxY: integer;
begin
    MinX := (ScreenWidth - WinZoneSize) div 2;
    MinY := (ScreenHeight - WinZoneSize) div 2;
    MaxX := MinX + 2;
    MaxY := MinY + 2;
    if (s.CurX >= MinX) and (s.CurY >= MinY) and
        (s.CurX <= MaxX) and (s.CurY <= MaxY) then
        HandleFinish(WinMessage)
end;

procedure CheckIfLost;
begin
    if (movements >= MaxMovements) or
        (ControlMotions >= MaxControlMotions) then
        HandleFinish(LoseMessage)
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
    movements := movements + 1;
    ShowScore;
    WrapStar(s);
    DrawStar(s);
    CheckIfWon(s)
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
    end;
    ControlMotions := ControlMotions + 1;
    ShowScore
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
    s.CurX := 1;
    s.CurY := 1;
    s.DirectionIndex := 3
end;

label
    StartGame;
var
    s: star;
    InputDir: direction;
    ticker, c: integer;
begin
	fpSystem('tput civis');
StartGame:
    randomize;
    clrscr;
    state := playing;
    movements := 0;
    ControlMotions := 0;
    ShowScore;
    InputDir := none;
    InitStar(s);
    DrawStar(s);
    DrawWinZone;
    ticker := MovesInPhase;
    while true do
    begin
        if KeyPressed then
        begin
            GetKey(c);
            case c of
                27: { escape }
                    break;
                97: { left }
                    InputDir := left;   
                100: { right }
                    InputDir := right;   
                119: { up }
                    InputDir := up;   
                115: { down }
                    InputDir := down;   
            end;
            if state <> playing then
                goto StartGame
        end;
        if state <> playing then
            continue;
        if (ticker >= MovesInPhase) and (InputDir <> none) then
        begin
            SetStarDirection(s, InputDir);
            InputDir := none;
            ticker := 0
        end
        else if random < Probability then
            DecodeRandomAndChangeDirection(s, 1 + random(2) * 2);
        delay(DelayDuration);
        MoveStar(s);
        CheckIfLost;
        ticker := ticker + 1
    end;
    clrscr;
	fpSystem('tput cnorm');
end.
