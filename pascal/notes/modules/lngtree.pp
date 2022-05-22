unit lngtree; { lngtree.pp }
interface

type
    TreeNodePtr = ^TreeNode;
    TreeNode = record
        data: longint;
        left, right: TreeNodePtr;
    end;

procedure AddToTree(var p: TreeNodePtr; value: longint; var ok: boolean);
function IsInTree(p: TreeNodePtr; value: longint): boolean;

implementation
{ uses Unix; } { import modules in implementation section }

type
    TreeNodePos = ^TreeNodePtr;

function SearchTree(var p: TreeNodePtr; value: longint): TreeNodePos;
begin
    if (p = nil) or (p^.data = value) then
        SearchTree := @p
    else if value < p^.data then
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

end.
