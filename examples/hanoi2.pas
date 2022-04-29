program hanoi2;

type
    pdisk = ^disk;
    disk = record
        k: integer;
        next: pdisk;
    end;
    RodsArray = array [1..3] of pdisk;

procedure MoveDiskItem(var RodFrom, RodTo: pdisk);
var
    tmp: pdisk;
begin
    tmp := RodFrom;
    RodFrom := RodFrom^.next;
    tmp^.next := RodTo;
    RodTo := tmp
end;

procedure MoveDisk(var rods: RodsArray; src, dst: integer);
begin
    writeln(rods[src]^.k, ': ', src, ' -> ', dst);
    MoveDiskItem(rods[src], rods[dst])
end;

procedure MoveSmallest(var rods: RodsArray; turn, total: integer);
var
    src, dst: integer;
begin
    if total mod 2 = 0 then
    begin
        src := (turn div 2) mod 3 + 1;
        dst := src + 1;
        if dst = 4 then
            dst := 1
    end
    else
    begin
        src := 3 - ((turn div 2 + 2) mod 3);
        dst := src - 1;
        if dst = 0 then
            dst := 3
    end;
    MoveDisk(rods, src, dst)
end;

procedure MoveLarger(var rods: RodsArray);
var
    src, dst, tmp: integer;
begin
    src := 1;
    dst := 2;
    if (rods[1] <> nil) and (rods[1]^.k = 1) then
        src := 3
    else
        if (rods[2] <> nil) and (rods[2]^.k = 1) then
            dst := 3;
    if (rods[src] = nil) or
        ((rods[dst] <> nil) and (rods[src]^.k > rods[dst]^.k)) then
    begin
        tmp := src;
        src := dst;
        dst := tmp
    end;
    MoveDisk(rods, src, dst)
end;

procedure solve(n: integer);
var
    rods: RodsArray;
    tmp: pdisk;
    i: integer;
    turn: integer;
begin
    for i := 1 to 3 do
        rods[i] := nil;
    for i := n downto 1 do
    begin
        new(tmp);
        tmp^.k := i;
        tmp^.next := rods[1];
        rods[1] := tmp
    end;
    turn := 1;
    while (rods[1] <> nil) or (rods[2] <> nil) do
    begin
        if turn mod 2 = 1 then
            MoveSmallest(rods, turn, n)
        else
            MoveLarger(rods);
        turn := turn + 1
    end
end;

var
    n, code: integer;
begin
    if ParamCount < 1 then
    begin
        writeln(ErrOutput, 'No parameters given');
        halt(1)
    end;
    val(ParamStr(1), n, code);
    if (code <> 0) or (n < 1) then
    begin
        writeln(ErrOutput, 'Invalid token count');
        halt(1)
    end;
    solve(n)
end.
