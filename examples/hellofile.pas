program HelloFile;
const
    message = 'Hello, world!';
    filename = 'hello.txt';
var
    f: text;
begin
    assign(f, filename);
    rewrite(f);
    writeln(f, message);
    close(f)
end.
