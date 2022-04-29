program SkipIndented;
var
    c: char;
    know, print: boolean;
begin
    know := false;
    print := false;
    while not eof do
    begin
        read(c);
        if c = #10 then
        begin
            if know and print then
                writeln;
            know := false
        end
        else
        begin
            if not know then
            begin
                print := (c <> ' ') and (c <> #9);
                know := true
            end;
            if print then
                write(c)
        end
    end
end.
