unit rbtree; { rbtree.pas }
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
procedure GetElement(p: TreeNodePtr; key: string;
    var out: TreeNodePtr; var ok: boolean);
procedure AddToTree(var p: TreeNodePtr; key: string; data: pointer);
procedure RemoveFromTree(var p: TreeNodePtr; key: string;
    var out: pointer; var code: integer);
procedure ClearTree(var p: TreeNodePtr);

implementation
uses sysutils;

type
    TreeNodePos = ^TreeNodePtr;

{$IFDEF DEBUG}
{ debuging print procedures }
procedure LogNodeInfo(p: TreeNodePtr);
begin
    if p <> nil then
        writeln('   key: ', p^.key)
    else
    begin
        writeln('   is nil');
        exit
    end;
    if p^.left <> nil then
        writeln('       left key: ', p^.left^.key)
    else
        writeln('       left is nil');
    if p^.right <> nil then
        writeln('       right key: ', p^.right^.key)
    else
        writeln('       right is nil');
    if p^.parent <> nil then
        writeln('       parent key: ', p^.parent^.key)
    else
        writeln('       parent is nil')
end;
{$ENDIF}

procedure SearchTree(var p: TreeNodePtr; key: string;
    var out, parent: TreeNodePos);
var
    res: longint;
begin
    if (p = nil) or (key = p^.key) then
    begin
        {$IFDEF DEBUG}
        if (p <> nil) then
            write('DEBUG: search, key exists');
        writeln;
        {$ENDIF}
        out := @p;
        if p <> nil then
            parent := @(p^.parent)
    end
    else
    begin
        parent := @p;
        res := CompareStr(key, p^.key);
        if res < 0 then
            SearchTree(p^.left, key, out, parent)
        else
            SearchTree(p^.right, key, out, parent)
    end
end;

function IsInTree(p: TreeNodePtr; key: string): boolean;
var
    out, parent: TreeNodePos;
begin
    SearchTree(p, key, out, parent);
    IsInTree := out^ <> nil
end;

procedure GetElement(p: TreeNodePtr; key: string;
    var out: TreeNodePtr; var ok: boolean);
var
    position, parent: TreeNodePos;
begin
    SearchTree(p, key, position, parent);
    ok := position^ <> nil;
    if ok then
        out := position^
end;

procedure AddToTreeSimple(var p, NewPos: TreeNodePtr; key: string;
    data: pointer);
var
    out, parent: TreeNodePos;
begin
    parent := nil;
    SearchTree(p, key, out, parent);
    if out^ = nil then
    begin
        new(out^);
        out^^.key := key;
        out^^.data := data;
        out^^.left := nil;
        out^^.right := nil;
        if parent <> nil then
            out^^.parent := parent^
        else
            out^^.parent := nil;
        out^^.color := red;
        {$IFDEF DEBUG}
        writeln('DEBUG: key -- ', out^^.key);
        write('parent info: ');
        LogNodeInfo(out^^.parent);
        {$ENDIF}
        if p = nil then
            p := out^
    end
    else
        out^^.data := data;
    NewPos := out^
end;

function grandparent(p: TreeNodePtr): TreeNodePtr;
begin
    if (p <> nil) and (p^.parent <> nil) then
        grandparent := p^.parent^.parent
    else
        grandparent := nil
end;

function uncle(p: TreeNodePtr): TreeNodePtr;
var
    g: TreeNodePtr;
begin
    g := grandparent(p);
    if g <> nil then
        if p^.parent = g^.left then
            uncle := g^.right
        else
            uncle := g^.left
    else
        uncle := nil
end;

procedure RotateLeft(p: TreeNodePtr; var root: TreeNodePtr);
var
    pivot: TreeNodePtr;
begin
    if (p = nil) or (p^.right = nil) then
        exit;
    {$IFDEF DEBUG}
    writeln('DEBUG: rotate left');
    LogNodeInfo(p);
    {$ENDIF}
    pivot := p^.right;
    pivot^.parent := p^.parent;
    if p^.parent <> nil then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: rotate not to root');
        {$ENDIF}
        if (p = p^.parent^.left) then
            p^.parent^.left := pivot 
        else
            p^.parent^.right := pivot
    end
    else
        root := pivot;
    p^.right := pivot^.left;
    if pivot^.left <> nil then
        pivot^.left^.parent := p;
    pivot^.left := p;
    p^.parent := pivot;
    {$IFDEF DEBUG}
    LogNodeInfo(p);
    {$ENDIF}
end;

procedure RotateRight(p: TreeNodePtr; var root: TreeNodePtr);
var
    pivot: TreeNodePtr;
begin
    if (p = nil) or (p^.left = nil) then
        exit;
    {$IFDEF DEBUG}
    writeln('DEBUG: rotate right');
    LogNodeInfo(p);
    {$ENDIF}
    pivot := p^.left;
    pivot^.parent := p^.parent;
    if p^.parent <> nil then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: rotate not to root');
        {$ENDIF}
        if (p = p^.parent^.left) then
            p^.parent^.left := pivot 
        else
            p^.parent^.right := pivot
    end
    else
        root := pivot;
    p^.left := pivot^.right;
    if pivot^.right <> nil then
        pivot^.right^.parent := p;
    pivot^.right := p;
    p^.parent := pivot;
    {$IFDEF DEBUG}
    LogNodeInfo(p);
    {$ENDIF}
end;

procedure InsertCase4(p: TreeNodePtr; var root: TreeNodePtr);
var
    g: TreeNodePtr;
begin
    {$IFDEF DEBUG}
    writeln('DEBUG: insert case 4');
    {$ENDIF}
    g := grandparent(p);
    {$IFDEF DEBUG}
    if p^.parent <> nil then
        writeln('    parent key: ', p^.parent^.key)
    else
        writeln('    parent is nil');
    write('    grandparent');
    LogNodeInfo(g);
    {$ENDIF}
    if p^.parent = g^.left then
        RotateRight(g, root)
    else if p^.parent = g^.right then
        RotateLeft(g, root);
    p^.parent^.color := black;
    g^.color := red
end;

procedure InsertCase3(p: TreeNodePtr; var root: TreeNodePtr);
var
    g: TreeNodePtr;
begin
    g := grandparent(p);
    if (p^.parent = g^.left) and (p = p^.parent^.right) then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: insert case 3');
        {$ENDIF}
        RotateLeft(p^.parent, root);
        p := p^.left
    end
    else if (p^.parent = g^.right) and (p = p^.parent^.left) then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: insert case 3');
        {$ENDIF}
        RotateRight(p^.parent, root);
        p := p^.right
    end;
    InsertCase4(p, root)
end;

procedure InsertCase2(p: TreeNodePtr; var root: TreeNodePtr);
var
    g, u: TreeNodePtr;
begin
    u := uncle(p);
    if (u <> nil) and (u^.color = red) then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: insert case 2');
        {$ENDIF}
        p^.parent^.color := black;
        u^.color := black;
        g := grandparent(p);
        g^.color := red;
        if g^.parent = nil then
            g^.color := black
        else if g^.parent^.color = red then
            InsertCase2(g, root)
    end
    else
        InsertCase3(p, root)
end;

procedure InsertCase1(p: TreeNodePtr; var root: TreeNodePtr);
begin
    if p^.parent = nil then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: insert case 1');
        {$ENDIF}
        p^.color := black
    end
    else if p^.parent^.color = red then
        InsertCase2(p, root)
    {$IFDEF DEBUG}
    else
        writeln('DEBUG: Insert case 1.5')
    {$ENDIF}
end;

procedure AddToTree(var p: TreeNodePtr; key: string; data: pointer);
var
    NewNode: TreeNodePtr;
begin
    AddToTreeSimple(p, NewNode, key, data);
    InsertCase1(NewNode, p);
    {$IFDEF DEBUG}
    writeln('DEBUG: new node color: ', NewNode^.color);
    if NewNode^.parent <> nil then
        writeln('DEBUG: new node parent color: ', NewNode^.parent^.color);
    writeln('DEBUG: root');
    LogNodeInfo(p)
    {$ENDIF}
end;

end.
