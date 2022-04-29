program Problem2_08; { 2_08.pas }
var
    i: integer;
begin
    for i := 0 to 9 do { a) }
        writeln('Hello');
    for i := 1 to 19 do { b) }
        writeln('Good bye');
    for i := 15 to 26 do { c) }
        writeln('abrakadabra');
    for i := 40 downto 26 do { d) }
        writeln('foobar');
    { e) cant be rewritten, because increment is not 1 or -1 }
    for i := 12 downto 22 do
        writeln('abcdefgh');
end.
