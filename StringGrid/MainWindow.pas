// Автор: Николай Ковалев
// Главное окно программы
unit MainWindow;


// Режим Object Free Pascal, длинные строки
{$mode objfpc}{$H+}

interface

uses
  // Стандартные импорты Lazarus
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, ActnList, StdActns, Menus,
  // Внутенняя логика
  Dates;

type

  // Класс главного окна
  { TMainWindowForm }

  TMainWindowForm = class(TForm)
    ActionList: TActionList;  // "Список действий" (в т.ч. стандартных)

    ActionFileOpen: TFileOpen;  // Стандартное действие "Открыть"
    ActionFileSave: TFileSaveAs;  // Стандартное действие "Сохранить как"

    MainMenu: TMainMenu;  // Строка меню
    MenuFile: TMenuItem;  // Меню "Файл"
    MenuFileSave: TMenuItem;  // Пункт "Сохранить как"
    MenuFileOpen: TMenuItem;  // Пункт "Открыть"

    StringGrid: TStringGrid;  // Таблица

    EditYear: TLabeledEdit;  // Поле "Год"
    EditMonth: TLabeledEdit;  // Поле "Месяц"
    EditDay: TLabeledEdit;  // Поле "Число"
       
    ButtonAdd: TButton;  // Кнопка "Добавить"
    ButtonFind: TButton;  // Кнопка "Найти"

    // Обработать создание экземпляра класса формы.
    // Инициализирует переменные.
    procedure FormCreate(Sender: TObject);

    // Обработать выбор файла при открытии
    procedure ActionFileOpenAccept(Sender: TObject);
    // Обработать выбор файла при сохранении
    procedure ActionFileSaveAccept(Sender: TObject);

    // Обработать клик по кнопке "Найти"
    procedure ButtonFindClick(Sender: TObject);
    // Обработать клик по кнопке "Добавить"
    procedure ButtonAddClick(Sender: TObject);
  private

  public

  end;

var
  MainWindowForm: TMainWindowForm;  // Экземпляр класса формы

implementation

var
  storage: TDateList;  // Хранилище записей базы данных

// Включить в `*.exe` файл ресурсов с формой
{$R *.lfm}


// Обработать создание экземпляра класса формы.
// Инициализирует переменные.
procedure TMainWindowForm.FormCreate(Sender: TObject);
begin
  // Заранее выделить память для массива
  setlength(storage.data, AllocIncrement);
  // Задать фактическое число элементов
  storage.reallength := 0;
end;


// Если в таблице не хватит строк для всех записей,
// увеличить их количество.
procedure MaybeIncRows;
begin
  // +1 и -1 из-за нулевой строки - заголовка таблицы
  if MainWindowForm.StringGrid.RowCount-1 < storage.reallength then
    MainWindowForm.StringGrid.RowCount := storage.reallength+1;
end;


// Отобразить одну запись (строку таблицы)
// n - индекс строки в массиве (начиная с 0)
procedure DisplayOneRow(n: integer);
begin
  // MaybeIncRows;  // Установить необходимое число строк

  // Строка 0 - заголовок. Используем n+1 в контексте StringGrid

  // год
  MainWindowForm.StringGrid.cols[0][n+1] := storage.data[n].year.toString;
  // месяц
  MainWindowForm.StringGrid.cols[1][n+1] := MonthToStr( TMonth(storage.data[n].month) );
  //MainWindowForm.StringGrid.cols[1][n] := storage[n].month.tostring;
  // число
  MainWindowForm.StringGrid.cols[2][n+1] := storage.data[n].day.toString;
end;


// Отобразить все записи (строки таблицы)
procedure DisplayAllRows;
var
  i: integer;
begin
  MaybeIncRows;  // Установить необходимое число строк

  // Отобразить каждый элемент массива (0..n-1)
  for i := 0 to storage.reallength-1 do
    displayonerow(i);
end;


// Методы класса главного окна

{ TMainWindowForm }


// Обработать клик по кнопке "Добавить"
procedure TMainWindowForm.ButtonAddClick(Sender: TObject);
var
  d: TDate;
begin
  // Заполнить запись с полей ввода
  d.year := StrToInt( EditYear.Text );
  d.month := StrToInt( EditMonth.Text );
  d.day := StrToInt( EditDay.Text );

  // Добавить запись в массив
  AddDateToList(d, storage);

  MaybeIncRows;  // Установить необходимое число строк
  DisplayOneRow(storage.reallength-1);  // Отобразить последнюю строку
end;


// Обработать клик по кнопке "Найти"
// Выделить строку с самой поздней датой
procedure TMainWindowForm.ButtonFindClick(Sender: TObject);
begin
  // +1 из-за нулевой строки - заголовка таблицы
  StringGrid.row := Dates.ArgMax(storage) + 1;
end;


// Обработать выбор файла при открытии
procedure TMainWindowForm.ActionFileOpenAccept(Sender: TObject);
begin
  // Загрузить данные из файла в массив storage
  //LoadDates(ActionFileOpen.dialog.FileName, storage, storagesize);
  storage := LoadDates(ActionFileOpen.Dialog.FileName);
  // Обновить таблицу
  DisplayAllRows;
end;


// Обработать выбор файла при сохранении
procedure TMainWindowForm.ActionFileSaveAccept(Sender: TObject);
begin
  // Сохранить данный из массива в файл
  StoreDates(ActionFileSave.Dialog.FileName, storage);
end;


end.

