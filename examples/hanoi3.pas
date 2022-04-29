program hanoi3;

function IntermRod(src, dst: integer) : integer;
begin
    if (src <> 1) and (dst <> 1) then
        IntermRod := 1
    else
    if (src <> 2) and (dst <> 2) then
        IntermRod := 2
    else
        IntermRod := 3
end;

procedure solve(n: integer);
type
    TaskState = (StClearing, StLargest, StFinal);
    ptask = ^task;
    task = record
        amount, src, dst: integer;
        state: TaskState;
        next: ptask;
    end;
var
    first, tmp: ptask;
begin
    new(first);
    first^.amount := n;
    first^.src := 1;
    first^.dst := 3;
    first^.state := StClearing;
    first^.next := nil;
    while first <> nil do
    begin
        case first^.state of
            StClearing:
                begin
                    first^.state := StLargest;
                    if first^.amount > 1 then
                    begin
                        new(tmp);
                        tmp^.src := first^.src;
                        tmp^.dst := IntermRod(first^.src, first^.dst);
                        tmp^.amount := first^.amount - 1;
                        tmp^.state := StClearing;
                        tmp^.next := first;
                        first := tmp
                    end
                end;
            StLargest:
                begin
                    first^.state := StFinal;
                    writeln(first^.amount, ': ', first^.src,
                            ' -> ', first^.dst);
                    if first^.amount > 1 then
                    begin
                        new(tmp);
                        tmp^.src := IntermRod(first^.src, first^.dst);
                        tmp^.dst := first^.dst;
                        tmp^.amount := first^.amount - 1;
                        tmp^.state := StClearing;
                        tmp^.next := first;
                        first := tmp
                    end
                end;
            StFinal:
                begin
                    tmp := first;
                    first := first^.next;
                    dispose(tmp)
                end
        end
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
