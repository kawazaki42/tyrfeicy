program main;

procedure putchar(c: Char);
external;
external 'c';

begin
  putchar('Z');
  writeln;
end.
