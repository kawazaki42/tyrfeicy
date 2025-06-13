// Библиотека на Паскале.
// Пример использования приведен на языке C в main.c
library my;
// NOTE: ключевое слово library вместо unit

// Реализации подпрограмм.
// NOTE: не работают ключевые слова interface, implementation.

// NOTE: ключевое слово cdecl устанавливает C-подобный стиль объявлений.
//       Для использования в других программах он должен совпадать!
//       cdecl - самый распространенный стиль.


// Сложить целые числа a и b.
function add_numbers(a, b: Integer): Integer;
cdecl;
begin
  add_numbers := a + b;
end;

// Вывести сообщение message.
procedure print_message(const message: PChar);
// NOTE: PChar - тип строки в стиле C.
//
//       Представляет собой указатель на начало
//       непрерывной последовательности символов.
//
//       Конец строки обозначается нуль-терминатором
//       т.е. `Chr(0)`, или же `#0`
//
//       С обычным String работает только сам Pascal.
cdecl;
begin
  writeln('Message: ', message);  // Pascal умеет работать с PChar
end;

{
// Такая реализация тоже работает!

// Вывести сообщение message.
procedure print_message(const message: array of Char);
var
  c: Char;
cdecl;
begin
  write('Message: ');
  
  for c in message do
  begin
    if c = #0 then
      break;
      
    write(c);
  end;

  writeln;
end;
}

// NOTE: exports вместо interface;
//       должен быть ПОСЛЕ реализации.
exports
  add_numbers, print_message;

end.
