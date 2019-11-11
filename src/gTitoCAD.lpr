program gTitoCAD;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uFormForm1, uFormPerspective,
  uFormDocument, uFrameDocumetExplorer, uFormViewProp, sketchDxf, sketchCore,
  sketchEditor, toLang, glob
  { you can add units after this };

{$R *.res}

begin
  //load defaults messages from units/toLang/toLang_defaults.pas
  glob.msg := tlang.Create();
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormPerspective, formPerspective);
  Application.CreateForm(TFormDocument, formDocument);
  Application.CreateForm(TFormViewProp, formViewProp);
  Application.Run;
end.

