program Problem2_26; { 2_26.pas }
uses crt;
const
    StarSymbol = '*';
    MaxCycles = 3;
    FrameDuration = 100;
type
    star = record
        CurX, CurY, dx, dy: integer;
    end;

procedure ShowStar(var s: star);
begin
	GotoXY(s.CurX, s.CurY);
	write(StarSymbol);
	GotoXy(1, 1)
end;

procedure HideStar(var s: star);
begin
	GotoXY(s.CurX, s.CurY);
	write(' ');
	GotoXY(1, 1)
end;

procedure ClampCoordinates(var s: star);
begin
	if s.CurX < 1 then
		s.CurX := 1;
	if s.CurX > ScreenWidth then
		s.CurX := ScreenWidth;
	if s.CurY < 1 then
		s.CurY := 1;
	if s.CurY > ScreenHeight then
		s.CurY := ScreenHeight
end;

procedure MoveStar(var s: star);
begin
	if (s.dx = 0) and (s.dy = 0) then
		exit;
	HideStar(s);
	s.CurX := s.CurX + s.dx;
	s.CurY := s.CurY + s.dy;
	ClampCoordinates(s);
	ShowStar(s)
end;

function StarIsNearEdge(var s: star; IsLeftEdge: boolean): boolean;
begin
    StarIsNearEdge := ((s.CurX >= ScreenWidth) and (not IsLeftEdge)) or
        ((s.CurX <= 1) and IsLeftEdge);
end;

procedure MoveStarAcrossScreen(var s: star; direction: integer);
begin
    if (direction <> 1) and (direction <> -1) then 
    begin
        writeln('Invalid direction for going across screen');
        halt(1)
    end;
    s.dx := direction;
    while not StarIsNearEdge(s, direction = -1) do
    begin
        MoveStar(s);
        delay(FrameDuration)
    end
end;
    
var
    s: star;
    counter: integer;
begin
    clrscr;
    s.CurY := ScreenHeight div 2;
    for counter := 1 to MaxCycles do
    begin
        MoveStarAcrossScreen(s, 1);
        MoveStarAcrossScreen(s, -1)
    end;
    clrscr
end.
