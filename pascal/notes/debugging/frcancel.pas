program frcancel; { frcancel.pas }
var
    x, y, TempX, TempY, gcd: integer;
begin { incorrect implementation for the sake of testing }
    read(x);
    read(y);
    TempX := x;
    TempY := y;
    while (TempX <> 0) and (TempY <> 0) do
    begin
        if TempX >= TempY then
            TempX := TempX mod TempY
        else
            TempY := TempY mod TempX;
        {$IFDEF DEBUG}
        writeln('DEBUG: x = ', TempX, ' y = ', TempY)
        {$ENDIF}
    end;
    if TempX = 0 then
        gcd := TempY
    else
        gcd := TempX;
    writeln(x div gcd, ' ', y div gcd)
end.
