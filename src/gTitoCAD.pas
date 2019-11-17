program gTitoCAD;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uForm1, uFormDocProps, uFormViewProp,
  glob;

{$R *.res}

begin
  //load defaults messages from units/toLang/toLang_defaults.pas

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1,glob.Form1);
  Application.CreateForm(TFormDocProps, glob.formDocProps);
  Application.CreateForm(TFormViewProp, glob.formViewProp);
  Application.Run;
end.

