library my;

function add_numbers(a, b: Integer): Integer;
begin
  add_numbers := a + b;
end;


procedure print_message(const message: PChar);
cdecl;
begin
  writeln('Message: ', message);
end;

{
procedure print_message(const message: array of Char);
var
  c: Char;
cdecl;
begin
  write('Message: ');
  
  for c in message do
  begin
    if c = chr(0) then
      break;
      
    write(c);
  end;

  writeln;
end;
}

exports
  add_numbers, print_message;

end.
