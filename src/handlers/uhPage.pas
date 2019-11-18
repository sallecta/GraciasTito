unit uhPage;

{$mode objfpc}

interface
uses forms, Dialogs, LCLType;


type
  tuhPage =  class
  public
    constructor Create; overload;
procedure doAdd(argForm: TObject);
procedure doRemove(argForm: TObject);
  end;


implementation
uses glob, uDoc, uForm1;

constructor tuhPage.Create;
begin
  inherited;
end;

procedure tuhPage.doAdd(argForm: TObject);
var
  argForm1: TForm1;
  newPage: TDoc.TPage;
begin
  argForm1 := TForm1(argForm);
  if argForm1.doc = nil then
    exit;  //there is no open document
  newPage:=argForm1.doc.AddPage;
  newPage.Name:=msg.Get('page');
  argForm1.Refresh;
end;

procedure tuhPage.doRemove(argForm: TObject);
var
  argForm1: TForm1;
begin
  argForm1 := TForm1(argForm);
  if argForm1.doc = nil then
    exit;
  if Application.MessageBox(PChar(msg.Get('AskPageDelete')+argForm1.doc.ActivePage.Name), '', (MB_ICONQUESTION + MB_YESNO)) <> IDYES then
    exit;
  argForm1.doc.RemovePage(argForm1.doc.ActivePage);
  argForm1.Refresh;
end;


begin

// creating instance


end.
