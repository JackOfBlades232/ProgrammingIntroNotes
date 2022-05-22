program UnitDemo; { unitdemo.pas }
uses rbtree;

var
    root: TreeNodePtr = nil;
    c: char;
    key: string;
    p: pointer;
    n: longint;
    ok: boolean;
begin
    while not eof do
    begin
        readln(c, key);
        case c of
            '?': begin
                if IsInTree(root, key) then
                    writeln('Yes!')
                else
                    writeln('No.')
            end;
            '*': begin
                GetElement(root, key, p, ok);
                if ok then
                    writeln('In tree, but cannot print out')
                else
                    writeln('Not in tree')
            end;
            '+': begin
                write('Input data: ');
                readln(n);
                p := @n;
                AddToTree(root, key, p);
                writeln('Successfully added')
            end;
            else
                writeln('Unknown command "', c , '"')
        end
    end
end.
