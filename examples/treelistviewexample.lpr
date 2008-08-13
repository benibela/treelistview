program treelistviewexample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms                  ,treelistviewexampleu
  { you can add units after this };

begin
  Application.Initialize;
  Application.CreateForm(TExampleForm,form);
  form.Visible:=true;

  Application.Run;
end.

