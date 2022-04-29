program UnitDemo;
uses lngtree;
var
    root: TreeNodePtr = nil;
    c: char;
    n: longint;
    ok: boolean;
begin
    while not eof do
    begin
        readln(c, n);
        case c of
            '?': begin
                if IsInTree(root, n) then
                    writeln('Yes!')
                else
                    writeln('No.')
            end;
            '+': begin
                AddToTree(root, n, ok);
                if ok then
                    writeln('Successfully added')
                else
                    writeln('Couldn''t add!')
            end;
            else
                writeln('Unknown command "', c, '"')
        end
    end
end.
