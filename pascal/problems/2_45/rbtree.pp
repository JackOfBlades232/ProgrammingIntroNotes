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

implementation
uses sysutils;

type
    TreeNodePos = ^TreeNodePtr;

procedure SearchTree(var p: TreeNodePtr; key: string;
    var out, parent: TreeNodePos);
var
    res: longint;
begin
    if (p = nil) or (key = p^.key) then
    begin
        {$IFDEF DEBUG}
        write('DEBUG: Assigned address');
        if (p <> nil) then
            write(', key exists');
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
        write('DEBUG: key -- ', out^^.key);
        if out^^.parent <> nil then
        begin
            write(', parent key -- ', out^^.parent^.key);
            if out^^.parent^.right <> nil then
                write(', parent right key -- ', out^^.parent^.right^.key);
            if out^^.parent^.left <> nil then
                write(', parent left key -- ', out^^.parent^.left^.key)
        end;
        writeln;
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

procedure RotateLeft(var p, root: TreeNodePtr);
var
    pivot: TreeNodePtr;
begin
    if (p = nil) or (p^.right = nil) then
        exit;
    {$IFDEF DEBUG}
    writeln('DEBUG: rotate left');
    {$ENDIF}
    pivot := p^.right;
    pivot^.parent := p^.parent;
    if p^.parent <> nil then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: rotate not to root');
        {$ENDIF}
        if (p = p^.parent^.left) then
            pivot := p^.parent^.left
        else
            pivot := p^.parent^.right
    end
    else
        root := pivot;
    p^.right := pivot^.left;
    if pivot^.left <> nil then
        pivot^.left^.parent := p;
    pivot^.left := p;
    p^.parent := pivot
end;

procedure RotateRight(var p, root: TreeNodePtr);
var
    pivot: TreeNodePtr;
begin
    if (p = nil) or (p^.left = nil) then
        exit;
    {$IFDEF DEBUG}
    writeln('DEBUG: rotate right');
    {$ENDIF}
    pivot := p^.left;
    pivot^.parent := p^.parent;
    if p^.parent <> nil then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: left rotate not to root');
        {$ENDIF}
        if (p = p^.parent^.left) then
            pivot := p^.parent^.left
        else
            pivot := p^.parent^.right
    end
    else
        root := pivot;
    p^.left := pivot^.right;
    if pivot^.right <> nil then
        pivot^.right^.parent := p;
    pivot^.right := p;
    p^.parent := pivot
end;

procedure InsertCase4(var p, root: TreeNodePtr);
var
    g: TreeNodePtr;
begin
    {$IFDEF DEBUG}
    writeln('DEBUG: insert case 4');
    {$ENDIF}
    g := grandparent(p);
    if p^.parent = g^.left then
        RotateRight(g, root)
    else if p^.parent = g^.right then
        RotateLeft(g, root);
    p^.parent^.color := black;
    g^.color := red
end;

procedure InsertCase3(var p, root: TreeNodePtr);
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

procedure InsertCase2(var p, root: TreeNodePtr);
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
        if p^.parent = nil then
            p^.color := black
        else if p^.parent^.color = red then
            InsertCase2(p, root)
    end
    else
        InsertCase3(p, root)
end;

procedure InsertCase1(var p, root: TreeNodePtr);
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
    NewPos: TreeNodePtr;
begin
    AddToTreeSimple(p, NewPos, key, data);
    InsertCase1(NewPos, p)
end;

end.
