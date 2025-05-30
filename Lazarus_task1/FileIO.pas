// Автор: Николай Ковалев
// Работа с файлами.
unit FileIO;

// Директивы компилятора. Объектный режим, 'длинные' строки.
{$mode ObjFPC}{$H+}

interface
// Прочитать из файла filename действительные числа A и B
procedure ReadAB(filename: string; var a, b: real);
// Записать в файл filename действительные числа A и B (в виде строк!)
procedure WriteAB(filename: string; a, b: string);
//procedure SaveReport(filename: string; report: string);

implementation
uses
  Classes, SysUtils;


// Прочитать из файла filename действительные числа A и B
procedure ReadAB(filename: string; var a, b: real);
var f: TextFile;
begin
  assign(f, filename);
  reset(f);

  readln(f, a);
  readln(f, b);

  close(f);
end;

// NOTE: не используется; отчет сохраняется через TMemo.Lines.SaveToFile
{procedure SaveReport(filename: string; report: string);
var f: TextFile;
begin
  assign(f, filename);
  rewrite(f);

  write(f, report);

  close(f);
end;}


// Записать в файл filename действительные числа A и B (в виде строк!)
procedure WriteAB(filename: string; a, b: string);
var f: TextFile;
begin
  assign(f, filename);
  rewrite(f);

  writeln(f, a);
  writeln(f, b);

  close(f);
end;

end.

