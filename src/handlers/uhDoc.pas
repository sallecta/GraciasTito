unit uhDoc;

{$mode objfpc}

interface


type
  tuhDoc =  class
  public
    constructor Create; overload;
procedure doNewReserved(argForm: TObject); 
procedure doProps(argForm: TObject);
procedure doClose(argForm: TObject);
  end;


implementation
uses glob, uDoc, uForm1, uFormDocProps;

constructor tuhDoc.Create;
begin
  inherited;
end;

procedure tuhDoc.doNewReserved(argForm: TObject);
var
  argForm1: TForm1;
begin 
  argForm1 := TForm1(argForm);
  argForm1.doc.Free;
  argform1.doc := TDoc.Create;
  argform1.doc.Name := msg.Get('document');
  argform1.doc.ActivePage.name := msg.Get('page');
  argForm1.showActivePage;
  argForm1.Refresh;
end;

procedure tuhDoc.doProps(argForm: TObject);
var
  argForm1: TForm1;
  locFormDocProp: TFormDocProps;
begin
  argForm1 := TForm1(argForm);
  locFormDocProp := TFormDocProps(glob.formDocProps);
  if argForm1.doc = nil then
    exit;
  if locFormDocProp.Exec(argForm1.doc) then
  begin
    argForm1.frameDocumentExplorer.Refresh;
  end;
end;

procedure tuhDoc.doClose(argForm: TObject);
var
  argForm1: TForm1;
begin
  argForm1 := TForm1(argForm);
  if argForm1.doc = nil then
    exit;  //nothing to doClose
  argForm1.doc.Destroy;
  argForm1.doc := nil;   //marks it as closed
  argForm1.Refresh;
end;


begin

// creating instance


end.
