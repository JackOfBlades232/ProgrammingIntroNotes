program MatchPattern;

function MatchIdx(var str, pat: string; idxs, idxp: integer): boolean;
var
    i: integer;
begin
    while true do
    begin
        if idxp > length(pat) then
        begin
            MatchIdx := idxs > length(str);
            exit
        end;
        if pat[idxp] = '*' then
        begin
            for i:=0 to length(str)-idxs+1 do
                if MatchIdx(str, pat, idxs+i, idxp+1) then
                begin
                    MatchIdx := true;
                    exit
                end;
            MatchIdx := false;
            exit
        end;
        if (idxs > length(str)) or
           ((str[idxs] <> pat[idxp]) and (pat[idxp] <> '?')) then
        begin
            MatchIdx := false;
            exit
        end;
        idxs := idxs + 1;
        idxp := idxp + 1;
    end
end;

function Match(str, pat: string): boolean;
begin
    Match := MatchIdx(str, pat, 1, 1)
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
