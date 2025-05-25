// Автор: Николай Ковалев
// Обработка дат.
unit Dates;

// Режим Object Free Pascal, длинные строки
{$mode ObjFPC}{$H+}

interface

uses
  // Стандартные импорты Lazarus
  Classes, SysUtils;

type
  // Перечисление с месяцами
  TMonth = (Jan=1, Feb, Mar, Apr, May, Jun,
            Jul, Aug, Sep, Oct, Nov, Dec);

  // Дата, состоящая из года, месяца, числа
  TDate = record
    year: word;
    month: byte;
    day: byte;
  end;

  // Динамический массив с предварительным выделением памяти
  TDateList = record
    data: array of TDate;  // Динамический массив
    reallength: integer;   // Фактическое число элементов
                           // Отличается от Length().
                           // Length(TDateList.data) - это количество выделенных ячеек
  end;


const
  // Инкремент выделения памяти
  // Число ячеек, выделяемых за раз
  AllocIncrement = 64;


// Получить название месяца
function MonthToStr(m: TMonth): String;

// Получить индекс самой поздней даты в массиве
function ArgMax(const dates: TDateList): integer;

// Выделить новые ячейки в массиве, если он заполнен.
//
// т. е. если кол-во элементов массива равно
// кол-ву выделенных ячеек памяти.
procedure MaybeIncSize(var list: TDateList);
// Добавить запись в массив
procedure AddDateToList(var date: TDate; var list: TDateList);

// Сохранить массив дат в файл
procedure StoreDates(fname: string; dates: TDateList);
// Получить массив дат из файла
function LoadDates(fname: string): TDateList;

implementation


// Выделить новые ячейки в массиве, если он заполнен.
//
// т. е. если кол-во элементов массива равно
// кол-ву выделенных ячеек памяти.
procedure MaybeIncSize(var list: TDateList);
begin
  if list.reallength = length(list.data) then
    // SetLength выделяет память
    setlength(list.data, list.reallength + AllocIncrement);
end;


// Добавить запись в массив
procedure AddDateToList(var date: TDate; var list: TDateList);
begin
  inc(list.reallength);  // Увеличить число хранимых элементов
  MaybeIncSize(list);  // Выделить память, если необходимо
  list.data[list.reallength-1] := date;  // Вставить запись в конец
end;


// Получить название месяца
function MonthToStr(m: TMonth): String;
begin
  case m of
    Jan: result := 'январь';
    Feb: result := 'февраль';
    Mar: result := 'март';
    Apr: result := 'апрель';
    May: result := 'май';
    Jun: result := 'июнь';
    Jul: result := 'июль';
    Aug: result := 'август';
    Sep: result := 'сентябрь';
    Oct: result := 'октябрь';
    Nov: result := 'ноябрь';
    Dec: result := 'декабрь';
    // Ошибка: неправильный месяц. Используем номер.
    else result := IntToStr( ord(m) );
  end;
end;


// Получить месяц по названию
function StrToMonth(s: string): TMonth;
begin
  case s of
    'январь': result := Jan;
    'февраль': result := Feb;
    'март': result := Mar;
    'апрель': result := Apr;
    'май': result := May;
    'июнь': result := Jun;
    'июль': result := Jul;
    'август': result := Aug;
    'сентябрь': result := Sep;
    'октябрь': result := Oct;
    'ноябрь': result := Nov;
    'декабрь': result := Dec;
  end;
end;


// Сохранить массив дат в файл
procedure StoreDates(fname: string; dates: TDateList);
var
  f: file of TDate;  // Типизированный файл дат
  i: integer;  // Итератор цикла
begin
  AssignFile(f, fname);  // Присвоить имя
  Rewrite(f);  // Открыть для перезаписи
  try
    // Записать все даты из массива
    for i := 0 to dates.reallength-1 do
      write(f, dates.data[i]); // Напр. диск полон
  finally  // Вызывается при ошибке (напр. диск полон)
           // и при нормальной работе (напр. для синхронизации
           // внутреннего буфера файловой системы, aka Flush)
    CloseFile(f);
  end;
end;


// Получить массив дат из файла
function LoadDates(fname: string): TDateList;
var
  f: file of TDate;  // Типизированный файл дат
begin
  AssignFile(f, fname);  // Присвоить имя
  Reset(f);  // Открыть для чтения
  try
    //result := TDateList.Create;
    //result.data.Create;
    SetLength(result.data, AllocIncrement);  // Выделить память
    result.reallength := 0;  // Инициализировать счетчик
    while not eof(f) do  // Пока в файле не кончились данные
    begin
      // Считать из файла элемент в конец массива
      read(f, result.data[result.reallength]);
      inc(result.reallength);  // увеличить счетчик фактических элементов
      MaybeIncSize(result);  // выделить память, если необходимо
    end;
  finally
    // Вызывается при ошибке (напр. диск полон)
    // и при нормальной работе (напр. для синхронизации
    // внутреннего буфера файловой системы, aka Flush)
    CloseFile(f);
  end;
end;


// Получить индекс самой поздней даты в массиве
function ArgMax(const dates: TDateList): integer;
var
  i: integer;  // Итератор цикла
begin
  argmax := 0;  // По умолчанию, первый элемент

  // Для каждого элемента массива
  for i := 0 to dates.reallength-1 do
  begin
    if dates.data[i].year < dates.data[argmax].year then
      // dates[i] меньше argmax (по году)
      continue;  // пропускаем

    if dates.data[i].year > dates.data[argmax].year then
    // dates[i] больше argmax (по году)
    begin
      argmax := i;  // обновляем временный показатель
      continue;  // проверяем следующую дату
    end;

    // dates[i] и argmax равны по году
    // Проверяем дальше

    if dates.data[i].month < dates.data[argmax].month then
      // dates[i] меньше argmax (по месяцу)
      continue;  // пропускаем

    if dates.data[i].month > dates.data[argmax].month then
    // dates[i] больше argmax (по месяцу)
    begin
      argmax := i;  // обновляем временный показатель
      continue;  // проверяем следующую дату
    end;

    // dates[i] и argmax равны по месяцу (и году)
    // Проверяем дальше

    if dates.data[i].day <= dates.data[argmax].day then
      // dates[i] меньше или равен argmax
      continue;

    // dates[i] строго больше argmax
    // обновляем временный показатель
    argmax := i;
    // проверяем следующую дату
  end;
end;

end.

