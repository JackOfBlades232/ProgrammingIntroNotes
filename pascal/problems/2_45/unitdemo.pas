program UnitDemo; { unitdemo.pas }
uses rbtree;

{$IFDEF TEST}
procedure DepthSearchNextStep(var cur, prev: TreeNodePtr; var finished: boolean);
begin
    if cur = nil then
    begin
        {$IFDEF DEBUG}
        writeln('DEBUG: invalid node for depth search');
        {$ENDIF}
        exit
    end
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
            {$IFDEF DEBUG}
            writeln('DEBUG: property 2 culprit node key: ', 
                cur^.key, ', color: ', cur^.color);
            if cur^.right = nil then
                writeln('    right node -- nil')
            else
                writeln('    right node key: ', cur^.right^.key,
                    ', color: ', cur^.right^.color);
            if cur^.left = nil then
                writeln('    left node -- nil')
            else
                writeln('    left node key: ', cur^.left^.key,
                    ', color: ', cur^.left^.color);
            {$ENDIF}
            SatisfiesProperty2 := false;
            exit
        end;
        DepthSearchNextStep(cur, prev, finished)
    end;
Success:
    SatisfiesProperty2 := true
end;

function SatisfiesProperty3(root: TreeNodePtr): boolean;
label
    Success;
var
    cur, prev: TreeNodePtr;
    finished: boolean;
    BlackCounter, benchmark: integer;
begin
    if root = nil then
        goto Success;
    cur := root;
    prev := nil;
    finished := false;
    BlackCounter := 1;
    benchmark := -1;
    while not finished do
    begin
        DepthSearchNextStep(cur, prev, finished);
        if (cur^.color = black) and (prev = cur^.parent) then
            BlackCounter := BlackCounter + 1
        else if (prev^.color = black) and (prev <> cur^.parent) then
            BlackCounter := BlackCounter - 1;
        if (cur^.left = nil) or (cur^.right = nil) then
            if benchmark = -1 then
                benchmark := BlackCounter
            else if BlackCounter <> benchmark then
            begin
                SatisfiesProperty3 := false;
                exit
            end
    end;
Success:
    SatisfiesProperty3 := true
end;
    

function TreeIsValid(root: TreeNodePtr): boolean;
begin
    TreeIsValid := SatisfiesProperty1(root) and
        SatisfiesProperty2(root) and SatisfiesProperty3(root)
end;

{$IFDEF DEBUG}
procedure CheckPropertiesSeparately(root: TreeNodePtr);
begin
    if SatisfiesProperty1(root) then
        writeln('DEBUG: Property 1 satisfied')
    else
        writeln('DEBUG: Property 1 NOT satisfied!');
    if SatisfiesProperty2(root) then
        writeln('DEBUG: Property 2 satisfied')
    else
        writeln('DEBUG: Property 2 NOT satisfied!');
    if SatisfiesProperty3(root) then
        writeln('DEBUG: Property 3 satisfied')
    else
        writeln('DEBUG: Property 3 NOT satisfied!')
end;
{$ENDIF}
{$ENDIF}

procedure SeekNotSpace(var c: char);
begin
    read(c);
    while (c = ' ') or (c = #9) or (c = #10) or (c = #13) do
        read(c)
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
    while not SeekEof do
    begin
        SeekNotSpace(c);
        readln(key);
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
                {$IFNDEF TEST}
                write('Input data: ');
                {$ENDIF}
                readln(n);
                p := @n;
                AddToTree(root, key, p);
                {$IFNDEF TEST}
                writeln('Successfully added')
                {$ENDIF}
            end;
            {$IFDEF TEST}
            '!': begin
                if TreeIsValid(root) then
                    writeln('TEST PASSED: Red-black tree is valid')
                else
                    writeln('TEST FAILED: Invalid tree!')
            end;
            {$IFDEF DEBUG}
            '~':
                CheckPropertiesSeparately(root);
            {$ENDIF}
            {$ENDIF}
            else
                writeln('Unknown command "', c , '"')
        end
    end
end.
