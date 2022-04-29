program TreeDemo;

type
    TreeNodePtr = ^TreeNode;
    TreeNode = record
        data: longint;
        left, right: TreeNodePtr;
    end;
    TreeNodePos = ^TreeNodePtr;



function SearchTree(var p: TreeNodePtr; val: longint): TreeNodePos;
begin
    if (p = nil) or (p^.data = val) then
        SearchTree := @p
    else
    if val < p^.data then
        SearchTree := SearchTree(p^.left, val)
    else
        SearchTree := SearchTree(p^.right, val)
end;

function AddToTree(var p: TreeNodePtr; val: longint): boolean;
var
    pos: TreeNodePos;
begin
    pos := SearchTree(p, val);
    if pos^ = nil then
    begin
        new(pos^);
        pos^^.data := val;
        pos^^.left := nil;
        pos^^.right := nil;
        AddToTree := true
    end
    else
        AddToTree := false
end;

function IsInTree(p: TreeNodePtr; val: longint): boolean;
begin
    IsInTree := SearchTree(p, val)^ <> nil
end;

var
    root: TreeNodePtr = nil;
    c: char;
    n: longint;

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
                if AddToTree(root, n) then
                    writeln('Successfully added')
                else
                    writeln('Couldn''t add!')
            end;
            else
                writeln('I don''t know such command! Try "+" or "?"')
        end
    end
end.
