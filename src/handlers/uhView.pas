unit uhView;

{$mode objfpc}

interface
uses forms, Dialogs, LCLType;


type
  tuhView =  class
  public
    constructor Create; overload;
procedure doView(argForm: TObject);
  end;


implementation
uses glob, uDoc, uForm1, uFormViewProp;

constructor tuhView.Create;
begin
  inherited;
end;

procedure tuhView.doView(argForm: TObject);
var
  argForm1: TForm1;
begin
  argForm1 := TForm1(argForm);
  TFormViewProp(glob.formViewProp).Exec(argForm1.DocumetExplorer_frameEditor);
end;


begin

// creating instance


end.
