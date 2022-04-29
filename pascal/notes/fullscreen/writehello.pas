program writehello; { writehello.pas }
uses crt;
const
    Message = 'Hello, world!';
    DelayInMs = 5000; { 5 sec }
var
    x, y: integer;
begin
    clrscr;
    x := (ScreenWidth - length(Message)) div 2;
    y := ScreenHeight div 2;
    GotoXY(x, y);
    write(Message);
    GotoXY(1, 1);
    delay(DelayInMs);
    clrscr
end.
