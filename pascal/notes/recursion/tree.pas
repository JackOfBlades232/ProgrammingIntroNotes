program BinaryTree; { tree.pas }
type
    TreeNodePos = ^TreeNodePtr;
    TreeNodePtr = ^TreeNode;
    TreeNode = record
        data: integer;
        left, right: TreeNodePtr;
    end;

function SumTree(p: TreeNodePtr): longint;
begin
    if p = nil then
        SumTree := 0
    else
        SumTree := p^.data + SumTree(p^.left) + SumTree(p^.right)
end;

function SearchTree(var p: TreeNodePtr; value: longint): TreeNodePos;
begin
    if (p = nil) or (p^.data = value) then
        SearchTree := @p
    else
    if value < p^.data then
        SearchTree := SearchTree(p^.left, value)
    else
        SearchTree := SearchTree(p^.right, value)
end;

procedure AddToTree(var p: TreeNodePtr; value: longint; var ok: boolean);
var
    position: TreeNodePos;
begin
    position := SearchTree(p, value);
    if position^ = nil then
    begin
        new(position^);
        position^^.data := value;
        position^^.left := nil;
        position^^.right := nil;
        ok := true
    end
    else
        ok := false
end;

function IsInTree(p: TreeNodePtr; value: longint): boolean;
begin
    IsInTree := SearchTree(p, value)^ <> nil
end;

var
    root: TreeNodePtr = nil;
    n: longint;
    ok: boolean = true;
begin
    write('Input number for search: ');
    readln(n);
    write('Input tree members: ');
    while (not SeekEoln) and ok do
    begin
        read(n);
        AddToTree(root, n, ok)
    end;
    writeln('Sum of tree: ', SumTree(root));
    if IsInTree(root, n) then
        writeln('It is in tree')
    else
        writeln('It is not in tree')
end.
