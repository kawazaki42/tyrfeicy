// Автор: Николай Ковалев
// Главное окно программы
unit MainWindow;

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
    ActionList: TActionList;
    ButtonAdd: TButton;
    ActionFileOpen: TFileOpen;
    ActionFileSave: TFileSaveAs;
    ButtonAdd1: TButton;
    EditYear: TLabeledEdit;
    EditMonth: TLabeledEdit;
    EditDay: TLabeledEdit;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileSave: TMenuItem;
    MenuFileOpen: TMenuItem;
    StringGrid: TStringGrid;
    procedure ButtonAdd1Click(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileSaveAccept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainWindowForm: TMainWindowForm;

implementation

var
  storage: TDateList;
  //storage: TDateArray;
  //storagesize: integer;

{$R *.lfm}

//procedure DisplayOneRow(n: integer, row: TDate);
procedure DisplayOneRow(n: integer);
begin
  // Строка 1 - заголовок
  // n - индекс с нуля

  (*if n > MainWindowForm.StringGrid.RowCount then
    MainWindowForm.StringGrid.Rowcount := n+1; *)
  MainWindowForm.StringGrid.cols[0][n+1] := storage.data[n].year.tostring;
  MainWindowForm.StringGrid.cols[1][n+1] := MonthToStr( TMonth(storage.data[n].month) );
  //MainWindowForm.StringGrid.cols[1][n] := storage[n].month.tostring;
  MainWindowForm.StringGrid.cols[2][n+1] := storage.data[n].day.tostring;
end;

procedure DisplayAllRows;
var
  i: integer;
begin
  if MainWindowForm.StringGrid.RowCount-1 < storage.reallength then
    MainWindowForm.StringGrid.RowCount := storage.reallength+1;
  for i := 0 to storage.reallength-1 do
    displayonerow(i);
end;

{ TMainWindowForm }

procedure TMainWindowForm.ButtonAddClick(Sender: TObject);
var
  d: TDate;
begin
  d.year := StrToInt( EditYear.Text );
  d.month := StrToInt( EditMonth.Text );
  d.day := StrToInt( EditDay.Text );

  inc(storage.reallength);
  MaybeIncSize(storage);
  storage.data[storage.reallength-1] := d;

  if storage.reallength > MainWindowForm.StringGrid.RowCount - 1 then
    MainWindowForm.StringGrid.RowCount := storage.reallength + 1;
  DisplayOneRow(storage.reallength-1);
end;

procedure TMainWindowForm.ButtonAdd1Click(Sender: TObject);
begin
  //StringGrid.row := Dates.ArgMax(storage, storagesize) + 1;
  StringGrid.row := Dates.ArgMax(storage) + 1;
end;

procedure TMainWindowForm.ActionFileOpenAccept(Sender: TObject);
begin
  //LoadDates(ActionFileOpen.dialog.FileName, storage, storagesize);
  storage := LoadDates(ActionFileOpen.dialog.FileName);
  DisplayAllRows;
end;

procedure TMainWindowForm.ActionFileSaveAccept(Sender: TObject);
begin
  StoreDates(ActionFileSave.dialog.FileName, storage);
end;

procedure TMainWindowForm.FormCreate(Sender: TObject);
begin
  //storagesize := 0;
  setlength(storage.data, 64);
  storage.reallength := 0;
end;

end.

