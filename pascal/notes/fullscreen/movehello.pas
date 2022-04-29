program MovingHello; { movehello.pas }
uses crt;
const
	Message = 'Hello, world!';

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

procedure ClampCoordinates(var x, y: integer);
begin
	if x < 1 then
		x := 1;
	if x > ScreenWidth then
		x := ScreenWidth;
	if y < 1 then
		y := 1;
	if y > ScreenHeight then
		y := ScreenHeight
end;

procedure ShowMessage(x, y: integer; msg: string);
begin
	GotoXY(x, y);
	write(msg);
	GotoXY(1, 1)
end;

procedure HideMessage(x, y: integer; msg: string);
var
	i: integer;
begin
	GotoXY(x, y);
	for i := 1 to length(msg) do
		write(' ');
	GotoXY(1, 1)
end;

procedure MoveMessage(var x, y: integer; msg: string; dx, dy: integer);
begin
	HideMessage(x, y, msg);
	x := x + dx;
	y := y + dy;
	ClampCoordinates(x, y);
	ShowMessage(x, y, msg)
end;

var
	CurX, CurY: integer;
	c: integer;
begin
	clrscr;
	CurX := (ScreenWidth - length(Message)) div 2;
	CurY := ScreenHeight div 2;
	ShowMessage(CurX, CurY, Message);
	while true do
	begin
		GetKey(c);
		if c > 0 then
			break;
		case c of
			-75: { left arrow }
				MoveMessage(CurX, CurY, Message, -1, 0);
			-77: { right arrow }
				MoveMessage(CurX, CurY, Message, 1, 0);
			-72: { up arrow }
				MoveMessage(CurX, CurY, Message, 0, -1);
			-80: { down arrow }
				MoveMessage(CurX, CurY, Message, 0, 1)
		end
	end;
	clrscr
end.
