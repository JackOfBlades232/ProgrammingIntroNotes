program RandomStars; { randstars.pas }
uses crt;
const
	DelayDuration = 20;
	ColorCount = 16;
var
	AllColors: array [1..ColorCount] of word = 
	(
		Black, Blue, Green, Cyan,
		Red, Magenta, Brown, LightGray,
		DarkGray, LightBlue, LightGreen, LightCyan,
		LightRed, LightMagenta, Yellow, White
	);
	x, y, col: integer;
	SaveTextAttr: integer;
begin
	SaveTextAttr := TextAttr;
	randomize; { generates random seed }
	clrscr;
	while not KeyPressed do
	begin
		x := random(ScreenWidth) + 1; { generates random integer from 0 to screen width }
		y := random(ScreenHeight) + 1;
		if (x = ScreenWidth) and (y = ScreenHeight) then
			continue;
		col := random(ColorCount) + 1;
		GotoXY(x, y);
		TextColor(AllColors[col]);
		write('*');
		delay(DelayDuration)
	end;
	write(#27'[0m');
	TextAttr := SaveTextAttr;
	clrscr
end.
