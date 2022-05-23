program UnitDemo; { unitdemo.pas }
uses rbtree;

procedure DepthSearchNextStep(var cur, prev: TreeNodePtr; var finished: boolean);
begin
    if cur = nil then
        exit
    else if prev = cur^.parent then
    begin
        prev := cur;
        if cur^.left <> nil then
            cur := cur^.left
        else if cur^.right <> nil then
            cur := cur^.right
        else if cur^.parent <> nil then
            cur := cur^.parent
        else
        begin
            finished := true;
            exit
        end
    end
    else if prev = cur^.left then
    begin
        prev := cur;
        if cur^.right <> nil then
            cur := cur^.right
        else if cur^.parent <> nil then
            cur := cur^.parent
        else
        begin
            finished := true;
            exit
        end
    end
    else if prev = cur^.right then
    begin
        prev := cur;
        if cur^.parent <> nil then
            cur := cur^.parent
        else
        begin
            finished := true;
            exit
        end
    end;
    finished := false
end;

function SatisfiesProperty1(root: TreeNodePtr): boolean;
begin
    SatisfiesProperty1 := (root = nil) or (root^.color = black)
end;

function NodeSatisfiesProperty2(p: TreeNodePtr): boolean;
begin
    NodeSatisfiesProperty2 := (p = nil) or (p^.color <> red) or 
        (((p^.left = nil) or (p^.left^.color = black))
            and ((p^.right = nil) or (p^.right^.color = black)))
end;

function SatisfiesProperty2(root: TreeNodePtr): boolean;
label
    Success;
var
    cur, prev: TreeNodePtr;
    finished: boolean;
begin
    if root = nil then
        goto Success;
    cur := root;
    prev := nil;
    finished := false;
    while not finished do
    begin
        if not NodeSatisfiesProperty2(cur) then
        begin
            SatisfiesProperty2 := false;
            exit
        end;
        DepthSearchNextStep(cur, prev, finished)
    end;
Success:
    SatisfiesProperty2 := true
end;

{ function SatisfiesProperty3(root: TreeNodePtr): boolean; }

function TreeIsValid(root: TreeNodePtr): boolean;
begin
    TreeIsValid := SatisfiesProperty1(root) and SatisfiesProperty2(root) { and
        SatisfiesProperty3(root) }
end;
 
var
    root: TreeNodePtr = nil;
    out: TreeNodePtr;
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
                GetElement(root, key, out, ok);
                if ok then
                    writeln('In tree, color: ', out^.color)
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
            {$IFDEF DEBUG}
            '!': begin
                if TreeIsValid(root) then
                    writeln('Red-black tree is valid')
                else
                    writeln('Invalid tree!')
            end;
            {$ENDIF}
            else
                writeln('Unknown command "', c , '"')
        end
    end
end.
