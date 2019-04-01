program TitoCad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, guiForm1, sketchEvalExpres, guiFormConfig, guiFrameCfgSynEdit,
  guiFrameCfgGeneral, guiFrameCfgPanCom, guiFormPerspective, guiFrameCfgView, guiFormProject,
  guiFrameProjExplorer, guiFormViewProp, sketchDxf, sketchCore, sketchEditor
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TConfig, Config);
  Application.CreateForm(TFormPerspective, FormPerspective);
  Application.CreateForm(TfrmProject, frmProject);
  Application.CreateForm(TFormPropView, FormPropView);
  Application.Run;
end.

