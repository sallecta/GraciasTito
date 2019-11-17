unit uhDraw;

{$mode objfpc}

interface
uses forms, Dialogs, LCLType;


type
  tuhDraw =  class
  public
    constructor Create; overload;
procedure doPolyLine(argForm: TObject);
  end;


implementation
uses glob, uDoc, uForm1, uFormViewProp;

constructor tuhDraw.Create;
begin
  inherited;
end;

procedure tuhDraw.doPolyLine(argForm: TObject);
var
  argForm1: TForm1;
  locPage: TDoc.TDocPage;
begin
  argForm1 := TForm1(argForm);
  if argForm1.doc = nil then
    exit;
  //locPage := argForm1.DocumetExplorer_page;
  locPage := argForm1.doc.ActivePage;
  locPage.frameEditor.ExecuteCommand('LINE');
  argForm1.Refresh;
end;


begin

// creating instance


end.
