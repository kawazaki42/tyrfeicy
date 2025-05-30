// Автор: Николай Ковалев
// Элементы управления главного окна.
unit MainWindow;

// Директивы компилятора. Объектный режим, 'длинные' строки.
{$mode objfpc}{$H+}

interface

uses
  // Стандартные импорты Lazarus.
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
  Menus, Buttons, ExtCtrls,
  // Использовать Math.RoundTo.
  // System.Round не поддерживает округление до определенной точности.
  Math,
  // Внутренняя логика программы (Model в MVC).
  Calculations,
  // Работа с файлами.
  FileIO;

type

  { TMainWindowForm }

  { Класс - один из главных концептов объектно-ориентированногопрограммирования
    (ООП). Это структура данных, содержащая т.н. поля (свойства, переменные) и
    методы (процедуры и функции, предназначенные для работы с ним.)}

  // Класс главного окна.
  TMainWindowForm = class(TForm)
    LabelHelp: TLabel;        // Текст справки
    Image: TImage;            // Картинка

    LabelInputA: TLabel;      // Подпись поля ввода числа A
    LabelInputB: TLabel;      // Подпись поля ввода числа B
    EditInputA: TEdit;        // Поле ввода для числа A
    EditInputB: TEdit;        // Поле ввода для числа B

    EditOutArith: TEdit;      // Поле вывода среднего арифметического
    EditOutGeom: TEdit;       // Поле вывода среднего геометрического
    LabelArith: TLabel;       // Подпись поля вывода среднего арифметического
    LabelGeom: TLabel;        // Подпись поля вывода среднего геометрического

    // Кнопка запуска вычислений
    ButtonCalc: TButton;

    // Кнопка переключения поля истории вычислений
    ToggleHistoryBox: TToggleBox;

    HistoryBox: TMemo;        // Поле истории вычислений (скрыто по умолчанию)

    MainMenu: TMainMenu;      // Строка меню


    MenuFile: TMenuItem;      // Меню "Файл"

    MenuItemOpen: TMenuItem;       // Открыть
    MenuItemSave: TMenuItem;       // Сохранить входные данные
    MenuItemSaveReport: TMenuItem; // Сохранить отчет
    MenuItemQuit: TMenuItem;       // Выйти

    MenuItemHelp: TMenuItem;   // Меню "Справка
    MenuItemAbout: TMenuItem;  // Пункт "О программе"


    // Диалоговые окна
    OpenDialog: TOpenDialog;  // Открыть
    SaveDialog: TSaveDialog;  // Сохранить


    // "Список действий" - компонент Lazarus, позволяющий легко привязать
    // сочетане клавиш к некоторому действию, т.е. обработчику событий.
    ActionList: TActionList;

    // Элементы ActionList.
    // Добавлены для привязки сочетаний клавиш.

    // Действие переключения поля истории вычислений.
    ActionToggleHist: TAction;
    // Действие запуска вычислений.
    ActionCalc: TAction;

    // Обработать нажатие кнопки "Расчет".
    procedure ButtonCalcClick(Sender: TObject);
    // Обработчик события отображения окна. Работает с его начальной высотой.
    procedure FormShow(Sender: TObject);
    // Обработать команду "О программе" из меню.
    procedure MenuItemAboutClick(Sender: TObject);
    // Обработать команду "Открыть" из меню.
    procedure MenuItemOpenClick(Sender: TObject);
    // Обработать команду "Выход" из меню.
    procedure MenuItemQuitClick(Sender: TObject);
    // Обработать команду "Сохранить входные данные" из меню.
    procedure MenuItemSaveClick(Sender: TObject);
    // Обработать команду "Сохранить отчет" из меню.
    procedure MenuItemSaveReportClick(Sender: TObject);
    // Обработать событие переключения истории
    procedure ActionToggleHistExecute(Sender: TObject);
    // Обработать нажатие переключателя истории ввода.
    procedure ToggleHistoryBoxChange(Sender: TObject);
  private

  public

  end;

var
  // Экземпляр класса главного окна.
  MainWindowForm: TMainWindowForm;

implementation

var
  // Начальная высота окна. Для обработки переключения истории
  BaseHeight: integer;

// Включить файл ресурсов в исполняемый файл.
{$R *.lfm}


{ TMainWindowForm }


// Получить ввод с TEdit, проверить на ошибки, выделить их.
//
// ctl - поле ввода (TEdit)
// lbl - надпись поля ввода (TLabel, необяз.)
// output - результат (var real)
//
// Если произошла ошибка, Меняет цвет текста в TEdit и делает его жирным.
// Иначе возвращает обычный цвет и начертание.
//
// Если lbl (TLabel) указано, таким же образом изменяет его текст.
//
// Возвращает true если ввод корректный, иначе false.
function handle_input(ctl: TEdit; var output: real): boolean;
begin
  result := trystrtofloat(ctl.Text, output);
  if result then
     // Число должно быть положительным.
     result := output > 0;
  if not result then  // Если ошибка, сменить цвет
  begin
     ctl.Font.Color := clMaroon;
     ctl.font.bold := true;
  end else           // Иначе вернуть цвет
  begin
     ctl.font.color := clDefault;
     ctl.font.bold := false;
  end;
end;


// Получить ввод с TEdit, проверить на ошибки, выделить их.
//
// ctl - поле ввода (TEdit)
// lbl - надпись поля ввода (TLabel, необяз.)
// output - результат (var real)
//
// Если произошла ошибка, Меняет цвет текста в TEdit и делает его жирным.
// Иначе возвращает обычный цвет и начертание.
//
// Если lbl (TLabel) указано, таким же образом изменяет его текст.
//
// Возвращает true если ввод корректный, иначе false.
function handle_input(ctl: TEdit; lbl: TLabel; var output: real): boolean;
overload;  // Перегруженная версия, т.е. с другим списком аргументов
begin
  result := handle_input(ctl, output);  // Обращаемся к версии без lbl
  if not result then              // Если ошибка, сменить цвет
  begin
     lbl.font.color := clmaroon;
     lbl.font.bold := true;
  end else                        // Иначе вернуть цвет
  begin
     lbl.font.color := cldefault;
     lbl.font.bold := false;
  end;
end;


// Обработать нажатие кнопки "Расчет".
procedure TMainWindowForm.ButtonCalcClick(Sender: TObject);
var
  a, b: real;
  sArith, sGeom: string;
  success: boolean;
begin
  A := 0;
  B := 0;

  // NOTE: логические операторы (в частности and)
  //       не вычисляют значения последующих операторов, если от них уже
  //       не зависит результат операции.
  // Например:
  //     False and TryStrToFloat(...)
  // никогда не вызовет TryStrToFloat.
  success := true;
  success := handle_input(EditInputA, LabelInputA, A) and success;
  success := handle_input(EditInputB, LabelInputB, B) and success;
  if not success then
  begin
    HistoryBox.Lines.Add('Error!');
    HistoryBox.Lines.Add('');  // Перевод строки
    exit;
  end;

  // Вывод в соответствующие экземпляры TEdit
  // (округление до 6 знаков после запятой).
  sArith := FloatToStr( roundto( AvgArith(a, b), -6) );
  sGeom := FloatToStr( roundto( AvgGeom(a, b), -6) );
  EditOutArith.text := sArith;
  EditOutGeom.text := sGeom;

  //HistoryBox.Lines.Add( stringofchar('-', 20) );
  HistoryBox.Lines.Add('A = '+FloatToStr(a));
  HistoryBox.Lines.Add('B = '+FloatToStr(b));
  HistoryBox.Lines.Add( stringofchar('-', 20) );
  HistoryBox.Lines.Add('AvgArith = '+sArith);
  HistoryBox.Lines.Add('AvgGeom  = '+sGeom);
  HistoryBox.Lines.Add('');  // Перевод строки
  //HistoryBox.Lines.Add('');
end;


// Обработчик события отображения окна. Работает с его начальной высотой.
procedure TMainWindowForm.FormShow(Sender: TObject);
begin
  BaseHeight := Height;
  Constraints.MinHeight := BaseHeight;
  Constraints.MaxHeight := BaseHeight;
end;


// Обработать событие переключения истории
procedure TMainWindowForm.ActionToggleHistExecute(Sender: TObject);
begin
  // Используем свойство кнопки, для правильного отображения её состояния
  ToggleHistoryBox.Checked := not ToggleHistoryBox.Checked;
end;


// Обработать команду "О программе" из меню.
procedure TMainWindowForm.MenuItemAboutClick(Sender: TObject);
const message = 'Калькулятор среднего арифметического и геометрического.'
                  + #13#10#13#10
                  + 'Версия 2025. Автор: Николай Ковалев';
begin
  // Всплывающее окно с сообщением.
  ShowMessage(message);
end;


// Обработать команду "Выход" из меню.
procedure TMainWindowForm.MenuItemQuitClick(Sender: TObject);
begin
  MainWindowForm.close;
end;


// Обработчики событий файловых команд используют метод Execute
// диалоговых окон. Он возвращает true если файл был выбран,
// иначе false. Если файл был выбран, он хранится в свойстве FileName.


// Обработать команду "Открыть" из меню.
procedure TMainWindowForm.MenuItemOpenClick(Sender: TObject);
var
  a, b: real;
begin
  if not OpenDialog.Execute then exit;
  FileIO.ReadAB(OpenDialog.filename, a, b);
  EditInputA.text := FloatToStr(a);
  EditInputB.text := FloatToStr(b);
end;


// Обработать команду "Сохранить входные данные" из меню.
procedure TMainWindowForm.MenuItemSaveClick(Sender: TObject);
begin
  if SaveDialog.execute then
    FileIO.WriteAB(SaveDialog.filename, EditInputA.text, EditInputB.text);
end;


// Обработать команду "Сохранить отчет" из меню.
procedure TMainWindowForm.MenuItemSaveReportClick(Sender: TObject);
begin
  if SaveDialog.execute then
    HistoryBox.Lines.SaveToFile( SaveDialog.filename );
end;


// Обработать нажатие переключателя истории ввода.
procedure TMainWindowForm.ToggleHistoryBoxChange(Sender: TObject);
begin
  HistoryBox.visible := not HistoryBox.visible;

  // Изменить макс/мин размер окна при переключении истории
  if not HistoryBox.visible then
  begin
    // Если история была отключена, уменьшить высоту окна
    Constraints.MaxHeight := BaseHeight;
    Height := BaseHeight;
  end
  else begin
    // Иначе увеличить ее и позволить изменение
    Constraints.MinHeight := BaseHeight + HistoryBox.Constraints.MinHeight + 8;
    Constraints.MaxHeight := 0;
    Height := Constraints.MinHeight;
  end;
end;


end.

