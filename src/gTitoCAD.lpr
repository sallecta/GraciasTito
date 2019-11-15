program gTitoCAD;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uFormForm1, 
  glob;

{$R *.res}

begin
  //load defaults messages from units/toLang/toLang_defaults.pas

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1,glob.Form1);
  //Application.CreateForm(TFormDocument, formDocument);
  //Application.CreateForm(TFormViewProp, formViewProp);
  Application.Run;
end.

