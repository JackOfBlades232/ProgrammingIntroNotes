program CancelFraction;


type
    num = int64;

function gcd(a, b: num) : num;
begin
    while (a <> 0) and (b <> 0) do
        if a > b then
            a := a mod b
        else
            b := b mod a;
    if a = 0 then
        gcd := b
    else
        gcd := a
end;


var
    x, y, d: num;
    sign: boolean;
begin
    readln(x, y);
    if x < 0 then
    begin
        x := -x;
        sign := true
    end
    else
        sign := false;
    if y < 0 then
    begin
        sign := not sign;
        y := -y
    end;
    d := gcd(x, y);
    if sign then
        write('-');
    writeln(x div d, ' ', y div d)
end.
