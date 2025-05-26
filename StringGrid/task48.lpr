// Автор: Николай Ковалев
// Главный "модуль". Точка входа в программу.
program task48;

// Режим Object Free Pascal, длинные строки
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainWindow, Dates
  { you can add units after this };  // Стандартные импорты Lazarus

// Включить в `*.exe` файл ресурсов с формой
{$R *.res}

begin
  // Требовать файл ресурсов
  RequireDerivedFormResource:=True;
  // Разрешить масштабирование
  Application.Scaled:=True;
  // Инициализация объекта приложения
  Application.Initialize;
  // Создать экземпляр формы в модуле с ней
  Application.CreateForm(TMainWindowForm, MainWindowForm);
  // Запустить
  Application.Run;
end.

