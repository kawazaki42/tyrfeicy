// Автор: Николай Ковалев
// Главный исполняемый файл программы.
program task4;

// Директивы компилятора. Объектный режим, 'длинные' строки.
{$mode objfpc}{$H+}

uses
  // Работа с многозадачностью на разных платформах.
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,   // this includes the LCL widgetset
                // Включает набор графических виджетов LCL
                // (Lazarus Component Library).

  Forms,        // Формы (виджеты верхнего уровня, т.е. окна)
  MainWindow,   // Модуль главного окна.
  Calculations, // Внутренняя логика программы.
  FileIO        // Работа с файлами.
  { you can add units after this }
  { здесь можно добавить модули };

// Включить файл ресурсов в исполняемый файл.
{$R *.res}


begin
  // Стандартная инициализация приложения.

  // Проверять файл ресурсов на наличие.
  RequireDerivedFormResource:=True;
  // Включить масштабирование (по DPI).
  Application.Scaled:=True;

  // Запустить программу!!
  Application.Initialize;
  Application.CreateForm(TMainWindowForm, MainWindowForm);
  Application.Run;
end.

