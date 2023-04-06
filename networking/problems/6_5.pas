program Problem6_5; { 6_5.pas, cause I can }
const
    MaxIpLen = 15;

procedure SplitIpToNumbers(ip: string; 
    var n1, n2, n3, n4: longword; var ok: boolean);
var
    i, i1, i2, i3, l1, l2, l3, l4: integer;
    code: word;
begin
    ok := false;
    if length(ip) > MaxIpLen then
        exit;

    { find all dots }
    i1 := -1;
    i2 := -1;
    i3 := -1;
    for i := 1 to length(ip) do
        if ip[i] = '.' then
            if i1 = -1 then
            begin
                i1 := i + 1;
                l1 := i - 1;
            end
            else if i2 = -1 then
            begin
                i2 := i + 1;
                l2 := i - i1
            end
            else if i3 = -1 then
            begin
                i3 := i + 1;
                l3 := i - i2
            end
            else
                exit;

    l4 := length(ip) - i3 + 1;

    val(copy(ip, 1, l1), n1, code);
    if code <> 0 then
        exit;
    val(copy(ip, i1, l2), n2, code);
    if code <> 0 then
        exit;
    val(copy(ip, i2, l3), n3, code);
    if code <> 0 then
        exit;
    val(copy(ip, i3, l4), n4, code);
    if code <> 0 then
        exit;

    ok := (n1 <= 255) and (n2 <= 255) and
          (n3 <= 255) and (n4 <= 255)
end;

procedure PrintSubnetAdr(adr: longword);
begin
    write(
        adr shr 24, '.', 
        (adr shr 16) and $FF, '.',
        (adr shr 8) and $FF, '.', 
        adr and $FF
    )
end;

procedure PrintAllSubnets(ip: longword);
var
    PrefixLen, PrevPrefixLen: integer;
    BitMask: longword = 1;
    SubnetMask: longword = not 0;
begin
    PrefixLen := 32;
    while PrefixLen > 0 do
    begin
        PrevPrefixLen := PrefixLen;
        while (PrefixLen > 0) and ((ip and BitMask) = 0) do
        begin
            BitMask := BitMask shl 1;
            SubnetMask := SubnetMask shl 1;
            PrefixLen := PrefixLen - 1
        end;
        if PrefixLen = 0 then
            PrintSubnetAdr(0)
        else
            PrintSubnetAdr(ip and SubnetMask);
        if PrevPrefixLen = PrefixLen then
            writeln('/', PrefixLen)
        else if PrevPrefixLen - PrefixLen = 1 then
            writeln('/', PrevPrefixLen, ',/', PrefixLen)
        else
            writeln('/', PrevPrefixLen, '../', PrefixLen);
        BitMask := BitMask shl 1;
        SubnetMask := SubnetMask shl 1;
        PrefixLen := PrefixLen - 1
    end
end;

var
    ip: longword;
    n1, n2, n3, n4: longword;
    ok: boolean;
begin
    if ParamCount < 1 then
    begin
        writeln(ErrOutput, 'Provide ip-address');
        halt(1)
    end;
    SplitIpToNumbers(ParamStr(1), n1, n2, n3, n4, ok);
    if not ok then
    begin
        writeln(ErrOutput, 'Invalid ip-address');
        halt(2)
    end;
    ip := (n1 shl 24) + (n2 shl 16) + (n3 shl 8) + n4;
    PrintAllSubnets(ip)
end.
