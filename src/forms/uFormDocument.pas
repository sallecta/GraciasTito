unit uFormDocument;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, ButtonPanel, Buttons, ComCtrls, Spin, Menus, sketchDocument;

type
  { TFormDocument }
  TFormDocument = class(TForm)
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
    aDocument: TDocument;
    procRefresh: procedure of object;
  public
    function Exec(argDoc: TDocument; soloRead: boolean = False): boolean;
    function ExecNew(argDoc: TDocument): boolean;
  end;

var
  formDocument: TFormDocument;

implementation
uses glob;

{$R *.lfm}
{ TFormDocument }
procedure TFormDocument.FormCreate(Sender: TObject);
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

procedure TFormDocument.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TFormDocument.OKButtonClick(Sender: TObject);
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

procedure TFormDocument.CancelButtonClick(Sender: TObject);
begin
  Accepted := False;
end;

function TFormDocument.Exec(argDoc: TDocument; soloRead: boolean = False): boolean;
begin
  aDocument := argDoc;

  TeditName.Text := aDocument.Name;
  TeditCreatedBy.Text := aDocument.createdBy;
  TMemoNotes.Text := aDocument.notes;

  ButtonPanel1.OKButton.Enabled := not soloRead;

  Self.ShowModal;
  Result := Accepted;
end;

function TFormDocument.ExecNew(argDoc: TDocument): boolean;

var
  docNameIndex: string;
begin
  docNameIndex := '1';
  argDoc.Name := msg.Get('DocumentDefaultName') + docNameIndex;
  Result := Exec(argDoc);
end;

end.
