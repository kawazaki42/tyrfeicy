// Автор: Николай Ковалев

// Интерфейс командной строки (CLI).
program t4cli;

// Параметры запуска (для Lazarus).

// Отображение стандартного вывода.
{%RunFlags MESSAGES+}

// Команда запуска.
// Здесь можно изменить аргументы.
{%RunCommand "$MakeExe($(EdFile))" 16 9}

// Команда компиляции.
{%BuildCommand "$(CompPath)" "$(EdFile)"}

uses
  sysutils,      // StrToFloat
  //lazutf8,       // UTF8ToSys
  calculations;  // Внутренняя логика.

// {$H+} // FIXME кодировка в Windows!


// Вывести справку.
procedure WriteHelp;
var progname: string;
begin
  // Выяснить имя программы (без содержащего каталога).
  progname := extractfilename(paramstr(0));

  {$ifdef windows} // Директива ("специальная команда") компилятора,
                   // выполняет действие только при компиляции на
                   // определенной платформе (MS Windows)
  // Исполнить процесс (внешнюю программу, команду).
  // chcp - команда MS-DOS и MS Windows, переклющающая кодировку.
  // (отсутствует на других платформах!)
  // 65001 - кодовая страница Unicode
  // (де-факто стандартная кодировка для практически всех алфавитов мира).
  executeprocess('c:\windows\system32\chcp.com', '65001');
  {$endif}

  writeln(progname, ' <a> <b>');
  writeln('Даны два действительных положительных числа.');
  writeln('Найти среднее арифметическое и среднее геометрическое этих чисел.');
  //halt
end;


var a, b: real;
// Точка входа. Главная программа.
begin
  // ParamCount - переменная, хранящая число аргументов командной строки.
  if paramcount = 0 then
  begin
    writehelp;
    exit;
  end;
  // ParamStr(n: integer) - n-й аргумент командной строки (в виде string!)
  case paramstr(1) of
    '-h', '--help', '/?': begin
      WriteHelp;
      exit;
    end;
  end;

  a := StrToFloat( ParamStr(1) );
  b := StrToFloat( ParamStr(2) );

  writeln('AvgArith = ', FloatToStr(AvgArith(a, b)) );
  writeln('AvgGeom  = ', FloatToStr(AvgGeom(a, b)) );
end.

