// Автор: Николай Ковалев
// Обработка дат.
unit Dates;

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
    data: array of TDate;
    reallength: integer;   // Количество хранимых элементов.
                           // Отличается от Length().
                           // Length(TDateList.data) - это количество выделенных ячеек
  end;


function MonthToStr(m: TMonth): String;
procedure StoreDates(fname: string; dates: TDateList);
function LoadDates(fname: string): TDateList;
function ArgMax(const dates: TDateList): integer;
procedure MaybeIncSize(var list: TDateList);

implementation

procedure MaybeIncSize(var list: TDateList);
begin
  if list.reallength = length(list.data) then
    setlength(list.data, list.reallength + 64);
end;

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
    else result := IntToStr( ord(m) );
  end;
end;

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


procedure StoreDates(fname: string; dates: TDateList);
var
  f: file of TDate;
  i: integer;
begin
  assign(f, fname);
  rewrite(f);
  try
    //write(f,
    for i := 0 to dates.reallength-1 do
      write(f, dates.data[i]); // Напр. диск полон
  finally
    close(f);  // Для flush!
  end;
end;

function LoadDates(fname: string): TDateList;
var
  f: file of TDate;
begin
  assign(f, fname);
  reset(f);
  try
    //size := 0;
    setlength(result.data, 64);
    result.reallength := 0;
    while not eof(f) do
    begin
      read(f, result.data[result.reallength]);
      inc(result.reallength);
      MaybeIncSize(result);
    end;
  finally
    close(f);
  end;
end;


function ArgMax(const dates: TDateList): integer;
var
  i: integer;
begin
  argmax := 0;
  for i := 0 to dates.reallength-1 do
  begin
    if dates.data[i].year < dates.data[argmax].year then continue;
    if dates.data[i].year > dates.data[argmax].year then
    begin
      argmax := i;
      continue;
    end;
    if dates.data[i].month < dates.data[argmax].month then continue;
    if dates.data[i].month > dates.data[argmax].month then
    begin
      argmax := i;
      continue;
    end;
    if dates.data[i].day <= dates.data[argmax].day then continue;

    argmax := i;
  end;
end;

end.

