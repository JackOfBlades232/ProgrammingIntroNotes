program enumerable;
type
	RainbowColors = (
		RcRed, RcOrange, RcYellow, RcGreen,
		RcBlue, RcIndigo, RcViolet
	);
	Signals = (SigRed, SigYellow, SigGreen);
var
	x: RainbowColors;
	y: Signals;
begin
	x := RcYellow;
	y := SigGreen;
	writeln('order of x and y: ', ord(x), ' ', ord(y));
	writeln('yellow > orange: ', x < RcOrange);
	writeln('rc green is after yellow: ', RcGreen = succ(x));
	writeln('sig yellow is before green: ', SigYellow = pred(y))
end.
