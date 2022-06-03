unit rbtree; { rbtree.pp }
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
procedure RemoveFromTree(var p: TreeNodePtr; key: string; var ok: boolean);
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

function IsBlack(p: TreeNodePtr): boolean;
begin
    IsBlack := (p = nil) or (p^.color = black)
end;

procedure SearchTree(var p: TreeNodePtr; key: string;
    var out, parent: TreeNodePos);
var
    res: longint;
begin
    if (p = nil) or (key = p^.key) then
    begin
        {$IFDEF DEBUG}
        if (p <> nil) then
            writeln('DEBUG: search, key exists');
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
    data: pointer; var IsNew: boolean);
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
        writeln('DEBUG: add key -- ', out^^.key);
        write('parent info: ');
        LogNodeInfo(out^^.parent);
        {$ENDIF}
        if p = nil then
            p := out^;
        IsNew := true
    end
    else
    begin
        out^^.data := data;
        IsNew := false
    end;
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

procedure InsertCase1(p: TreeNodePtr; var root: TreeNodePtr); forward;

procedure InsertCase2(p: TreeNodePtr; var root: TreeNodePtr);
var
    g, u: TreeNodePtr;
begin
    u := uncle(p);
    if not IsBlack(u) then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: insert case 2');
        {$ENDIF}
        p^.parent^.color := black;
        u^.color := black;
        g := grandparent(p);
        g^.color := red;
        InsertCase1(g, root)
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
    IsNew: boolean;
begin
    AddToTreeSimple(p, NewNode, key, data, IsNew);
    if IsNew then
        InsertCase1(NewNode, p);
    {$IFDEF DEBUG}
    writeln('DEBUG: new node color: ', NewNode^.color);
    if NewNode^.parent <> nil then
        writeln('DEBUG: new node parent color: ', NewNode^.parent^.color);
    writeln('DEBUG: root');
    LogNodeInfo(p)
    {$ENDIF}
end;

function child(p: TreeNodePtr): TreeNodePtr;
begin
    if (p <> nil) and (p^.left <> nil) then
        child := p^.left
    else if (p <> nil) and (p^.right <> nil) then
        child := p^.right
    else
        child := nil
end;

function sibling(p, parent: TreeNodePtr): TreeNodePtr;
begin
    if parent = nil then
        sibling := nil
    else if p = parent^.right then
        sibling := parent^.left
    else
        sibling := parent^.right
end;

procedure RemoveCase4(p, parent: TreeNodePtr; var root: TreeNodePtr);
var 
    s: TreeNodePtr;
begin
    {$IFDEF DEBUG}
    writeln('DEBUG: remove case 4');
    {$ENDIF}
    s := sibling(p, parent);
    s^.color := parent^.color;
    parent^.color := black;
    if p = parent^.left then
    begin
        s^.right^.color := black;
        RotateLeft(parent, root)
    end
    else     
    begin
        s^.left^.color := black;
        RotateRight(parent, root)
    end
end;

procedure RemoveCase3(p, parent: TreeNodePtr; var root: TreeNodePtr);
var 
    s: TreeNodePtr;
begin
    s := sibling(p, parent);
    if (s <> nil) and (p = parent^.left) and 
        (not IsBlack(s^.left)) and IsBlack(s^.right) then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: remove case 3');
        {$ENDIF}
        s^.color := red;
        s^.left^.color := black;
        RotateRight(s, root)
    end
    else if (s <> nil) and (p = parent^.right) and
        (not IsBlack(s^.right)) and IsBlack(s^.left) then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: remove case 3');
        {$ENDIF}
        s^.color := red;
        s^.right^.color := black;
        RotateLeft(s, root)
    end;
    RemoveCase4(p, parent, root)
end;

procedure RemoveCase1(p, parent: TreeNodePtr; var root: TreeNodePtr); forward;

procedure RemoveCase2(p, parent: TreeNodePtr; var root: TreeNodePtr);
var 
    s: TreeNodePtr;
begin
    s := sibling(p, parent);
    if (s <> nil) and IsBlack(s^.left) and IsBlack(s^.right) then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: remove case 2');
        {$ENDIF}
        s^.color := red;
        if parent^.color = black then
            RemoveCase1(parent, parent^.parent, root) 
        else
            parent^.color := black
    end
    else
        RemoveCase3(p, parent, root)
end;

procedure RemoveCase1(p, parent: TreeNodePtr; var root: TreeNodePtr);
var 
    s: TreeNodePtr;
begin
    s := sibling(p, parent);
    if not IsBlack(s) then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: remove case 1');
        {$ENDIF}
        s^.color := black;
        parent^.color := red;
        if p = parent^.left then
            RotateLeft(parent, root)
        else
            RotateRight(parent, root)
    end;
    RemoveCase2(p, parent, root)
end;

procedure RemoveCase0(p, parent: TreeNodePtr; var root: TreeNodePtr);
begin
    if not IsBlack(p) then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: remove case 0');
        {$ENDIF}
        p^.color := black
    end
    else if parent <> nil then
        RemoveCase1(p, parent, root)
    {$IFDEF DEBUG}
    else
        writeln('DEBUG: remove case 0.5')
    {$ENDIF}
end;

procedure ReplaceWithChild(var p, parent: TreeNodePtr; var root: TreeNodePtr);
var
    tmp: TreeNodePtr;
begin
    tmp := p;
    parent := p^.parent;
    p := child(p);
    if p <> nil then
        p^.parent := tmp^.parent;
    if tmp^.parent = nil then
        root := p
    else
        if tmp = tmp^.parent^.left then
            tmp^.parent^.left := p
        else
            tmp^.parent^.right := p;
    dispose(tmp)
end;

procedure RemoveBase(p: TreeNodePtr; var root: TreeNodePtr);
var
    color: NodeColor;
    tmp, parent: TreeNodePtr;
begin
    {$IFDEF DEBUG}
    writeln('DEBUG: remove key -- ', p^.key);
    write('parent info: ');
    LogNodeInfo(p^.parent);
    {$ENDIF}
    if (p^.left <> nil) and (p^.right <> nil) then
    begin
        tmp := p^.left;
        while tmp^.right <> nil do
            tmp := tmp^.right;
        p^.key := tmp^.key;
        p^.data := tmp^.data;
        p := tmp
    end;
    {$IFDEF DEBUG}
    writeln('DEBUG: real remove key -- ', p^.key, ', color: ', p^.color);
    LogNodeInfo(p);
    {$ENDIF}
    color := p^.color;
    ReplaceWithChild(p, parent, root);
    {$IFDEF DEBUG}
    writeln('   color after replacement: ', color);
    {$ENDIF}
    if color = black then
        RemoveCase0(p, parent, root)
end;

procedure RemoveFromTree(var p: TreeNodePtr; key: string; var ok: boolean);
var
    out, parent: TreeNodePos;
begin
    parent := nil;
    SearchTree(p, key, out, parent);
    if out^ = nil then
        ok := false
    else
    begin
        ok := true;
        {$IFDEF DEBUG}
        writeln('DEBUG: removed node color: ', out^^.color); 
        {$ENDIF}
        RemoveBase(out^, p)
    end
end;

procedure ClearTree(var p: TreeNodePtr);
var
    tmp, parent: TreeNodePtr;
begin
    tmp := p;
    while p <> nil do
        ReplaceWithChild(tmp, parent, p)
end;
        
end.
