program MatchPattern; { match_pt.pas }

function MatchIdx(var st, pt: string; idxs, idxp: integer): boolean;
var
    i: integer;
begin
    while true do
    begin
        if idxp > length(pt) then
        begin
            MatchIdx := idxs > length(st);
            exit
        end;
        if pt[idxp] = '*' then
        begin
            for i := 0 to length(st) - idxs + 1 do
                if MatchIdx(st, pt, idxs + i, idxp + 1) then
                begin
                    MatchIdx := true;
                    exit
                end;
            MatchIdx := false;
            exit
        end;
        if (idxs > length(st)) or
           ((st[idxs] <> pt[idxp]) and (pt[idxp] <> '?')) then
        begin
            MatchIdx := false;
            exit
        end;
        idxs := idxs + 1;
        idxp := idxp + 1
    end
end;

function Match(st, pt: string): boolean;
begin
    Match := MatchIdx(st, pt, 1, 1)
end;

begin
    if ParamCount < 2 then
    begin
        writeln(ErrOutput, 'Two parameters expected');
        halt(1)
    end;
    if Match(ParamStr(1), ParamStr(2)) then
        writeln('yes')
    else
        writeln('no')
end.
