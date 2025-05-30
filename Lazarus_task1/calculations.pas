// Автор: Николай Ковалев
// Внутренняя логика программы.
unit Calculations;

// Директивы компилятора. Объектный режим, 'длинные' строки.
{$mode ObjFPC}{$H+}

interface

// Вычислить среднее арифметическое двух чисел.
function AvgArith(a, b: real): real;
// Вычислить среднее геометрическое двух чисел.
function AvgGeom(a, b: real): real;


implementation

uses
  // Стандартные импорты Lazarus. ООП, утилиты Delphi.
  Classes, SysUtils;

// Вычислить среднее арифметическое двух чисел.
function AvgArith(a, b: real): real;
begin
  result := (a + b) / 2;
end;

// Вычислить среднее геометрическое двух чисел.
function AvgGeom(a, b: real): real;
begin
  result := sqrt(a * b);
end;

end.

