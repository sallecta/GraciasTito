unit uhDoc;

{$mode objfpc}

interface


type
  tuhDoc =  class
  public
    constructor Create; overload;
procedure doNewCustom(argForm: TObject);
procedure doClose(argForm: TObject);
  end;


implementation
uses glob, sketchDocument, uformForm1;

constructor tuhDoc.Create;
begin
  inherited;
end;

procedure tuhDoc.doNewCustom(argForm: TObject);
var
  argForm1: TForm1;
begin 
  argForm1 := TForm1(argForm);
  argForm1.doc.Free;
  argform1.doc := TDocument.Create;
  argForm1.doc.OnChangePersp := @argForm1.curDocumentChangeView;
  argForm1.doc.OnChangeActivePage := @argForm1.curDocumentChangeActivePage;
  argForm1.doc.OnMouseMoveVirt := @argForm1.curDocumentMouseMoveVirt;
  argForm1.doc.OnChangeState := @argForm1.curDocumentChangeState;
  argForm1.doc.ActivePage.View.OnSendMessage := @argForm1.curDocumentActivePageViewSendMessage;
  argForm1.curDocumentChangeActivePage;  //for priv_Refresh in your viewer
  argForm1.doc.ActivePage.View.InitView;  //start the axes
  argForm1.doc.SaveFile;
  argForm1.Refresh;
end;

procedure tuhDoc.doClose(argForm: TObject);
var
  argForm1: TForm1;
begin
  argForm1 := TForm1(argForm);
  if argForm1.doc = nil then
    exit;  //nothing to doClose
  //check if there is a modified document
  if argForm1.MessageSaveChanges = glob.BUT_CANCEL then
    exit;
  argForm1.doc.Destroy;
  argForm1.doc := nil;   //marks it as closed
  argForm1.Refresh;
end;


begin

// creating instance


end.
