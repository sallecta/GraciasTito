program TitoCad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, TypeForm1, EvalExpres, FormConfig, FrameCfgSynEdit,
  FrameCfgGeneral, FrameCfgPanCom, TypeFormPerspective, FrameCfgVista, FormProject,
  guiFrameProjExplorer, TypeFormPropView, guiDrawObjects, DefObjGraf, VisGraf3D
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

