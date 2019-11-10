program GraciasTito;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, guiForm1, sketchEvalExpres, guiFormPerspective,
  guiFormProject, guiFrameProjExplorer, guiFormViewProp, sketchDxf, sketchCore,
  sketchEditor, toLang, glob
  { you can add units after this };

{$R *.res}

begin
  //load defaults messages from units/toLang/toLang_defaults.pas
  glob.msg := tlang.Create();
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormPerspective, FormPerspective);
  Application.CreateForm(TfrmProject, frmProject);
  Application.CreateForm(TFormPropView, FormPropView);
  Application.Run;
end.

