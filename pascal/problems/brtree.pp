unit BlackRedTree; { brtree.pas }
interface

type
    NodeColor = (red, black);
    TreeNodePtr = ^TreeNode;
    TreeNode = record
        key: string;
        data: pointer;
        left, right, parent: TreeNodePtr;
        color: NodeColor;
    end;

function IsInTree(p: TreeNodePtr; key: string): boolean;
procedure AddToTree(var p: TreeNodePtr; key: string; data: pointer);
procedure RemoveFromTree(var p: TreeNodePtr; key: string;
    var out: pointer; var success: boolean);

implementation

type
    TreeNodePos = ^TreeNodePtr;

procedure SearchTree(var p: TreeNodePtr; key: string; var out, parent: TreeNodePos);
var
    res: longint;
begin
    if (p = nil) or (p^.key = key) then
        out := @p;
        if p = nil then
            parent := nil
        else
            parent := p^.parent
    else
    begin
        res := CompareStr(key, p^.key);
        if res < 0 then
            SearchTree(p^.left, key, out, parent)
        else
            SearchTree(p^.right, key, out, parent)
    end
end;

function IsInTree(var p: TreeNodePtr; key: string): boolean;
var
    out, parent: TreeNodePos;
begin
    SearchTree(p, key, out, parent);
    IsInTree = out^ <> nil
end;

procedure AddToTreeSimple(var p: TreeNodePtr; key: string; data: pointer; var out: TreeNodePos);
var
    parent: TreeNodePos;
begin
    SearchTree(p, key, out, parent);
    if out^ = nil then
    begin
        new(out^);
        out^^.key := key;
        out^^.data := data;
        out^^.left := nil;
        out^^.rigth := nil;
        out^^.parent := parent;
        out^^.color := red
    end
    else
        out^^.data := data
end;

function grandparent(var p: TreeNodePtr): TreeNodePtr;
begin
    if (p <> nil) and (p^.parent <> nil) then
        grandparent := p^.parent^.parent
    else
        grandparent := nil
end;

function uncle(var p: TreeNodePtr): TreeNodePtr;
var
    g: TreeNodePtr;
begin
    g := grandparent(p);
    if g <> nil then
        if p^.parent := g^.left then
            uncle := g^.right
        else
            uncle := g^.left
    else
        uncle := nil
end;

procedure RotateLeft(var p: TreeNodePtr);
var
    pivot: TreeNodePtr;
begin
    if (p = nil) or (p^.rigth = nil) then
        exit;
    pivot := p^.right;
    pivot^.parent := p^.parent;
    if (p = p^.parent^.left) then
        pivot := p^.parent^.left
    else
        pivot := p^.parent^.right;
    p^.right := pivot^.left;
    if pivot^.left <> nil then
        pivot^.left^.parent := p;
    pivot^.left := p;
    p^.parent := pivot
end;

procedure RotateRight(var p: TreeNodePtr);
var
    pivot: TreeNodePtr;
begin
    if (p = nil) or (p^.left = nil) then
        exit;
    pivot := p^.left;
    pivot^.parent := p^.parent;
    if (p = p^.parent^.left) then
        pivot := p^.parent^.left
    else
        pivot := p^.parent^.right;
    p^.left := pivot^.right;
    if pivot^.right <> nil then
        pivot^.right^.parent := p;
    pivot^.right := p;
    p^.parent := pivot
end;

procedure InsertCase1(var p: TreeNodePtr);
begin
    if p^.parent = nil then
        p^.color := black
    else
        InsertCase2(p)
end;

procedure InsertCase2(var p: TreeNodePtr);
begin
    if p^.parent^.color = red then
        InsertCase3(p)
end;

procedure InsertCase3(var p: TreeNodePtr);
var
    g, u: TreeNodePtr;
begin
    u := uncle(p);
    if (p^.parent^.color = red) and (u^.color = red) then
    begin
        p^.parent^.color := black;
        u^.color := black;
        g := grandparent(p);
        g^.color := red;
        InsertCase1(g)
    end
    else
        InsertCase4(p)
end;
