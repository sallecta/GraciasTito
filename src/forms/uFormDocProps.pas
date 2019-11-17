unit uFormDocProps;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, ButtonPanel, Buttons, ComCtrls, Spin, Menus, uDoc;

type
  { TFormDocProps }
  TFormDocProps = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    TeditName: TEdit;
    Label5: TLabel;
    TeditCreatedBy: TEdit;
    Label8: TLabel;
    TMemoNotes: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    Accepted: boolean;
    ErrorData: boolean;
    aDocument: TDoc;
    procRefresh: procedure of object;
  public
    function Exec(argDoc: TDoc): boolean;
    function ExecNew(argDoc: TDoc): boolean;
  end;

var
  formDocProps: TFormDocProps;

implementation
uses glob;

{$R *.lfm}
{ TFormDocProps }
procedure TFormDocProps.FormCreate(Sender: TObject);
begin
  Accepted := False;

  // captions
  Caption:=msg.get('document');
  TabSheet1.caption:=msg.get('general'); 
  Label1.caption:=msg.get('name');
  //
  TabSheet3.caption:=msg.get('notes');
  Label5.caption:=msg.get('author');
  Label8.caption:=msg.get('notes');
end;

procedure TFormDocProps.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TFormDocProps.OKButtonClick(Sender: TObject);
begin
  //Validate
  if trim(TeditName.Text) = '' then
  begin
    ShowMessage('Error en Cliente: ' + TeditName.Text);
    TeditName.Visible := True;
    PageControl1.ActivePage := TTabSheet(TeditName.Parent);
    TeditName.SetFocus;
    ErrorData := True;
    Accepted := True;
    exit;
  end;
  aDocument.Name := TeditName.Text;
  aDocument.notes := TMemoNotes.Text;
  aDocument.createdBy := TeditCreatedBy.TextHint;
  Accepted := True;
end;

procedure TFormDocProps.CancelButtonClick(Sender: TObject);
begin
  Accepted := False;
end;

function TFormDocProps.Exec(argDoc: TDoc): boolean;
begin
  aDocument := argDoc;

  TeditName.Text := aDocument.Name;
  TeditCreatedBy.Text := aDocument.createdBy;
  TMemoNotes.Text := aDocument.notes;

  Self.ShowModal;
  Result := Accepted;
end;

function TFormDocProps.ExecNew(argDoc: TDoc): boolean;

var
  docNameIndex: string;
begin
  docNameIndex := '1';
  argDoc.Name := msg.Get('DocumentDefaultName') + docNameIndex;
  Result := Exec(argDoc);
end;

end.
