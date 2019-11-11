unit uFormProject;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, ButtonPanel, Buttons, ComCtrls, Spin, Menus, sketchDocument, glob;

type
  { TFormProject }
  TFormProject = class(TForm)
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
    aProject: TProject;
    procRefresh: TEvRefresh;
  public
    function Exec(argProj: TProject; soloRead: boolean = False): boolean;
    function ExecNew(argProj: TProject): boolean;
  end;

var
  formProject: TFormProject;

implementation

{$R *.lfm}
{ TFormProject }
procedure TFormProject.FormCreate(Sender: TObject);
begin
  Accepted := False;

  // captions
  Caption:=msg.get('project');
  TabSheet1.caption:=msg.get('general'); 
  Label1.caption:=msg.get('name');
  //
  TabSheet3.caption:=msg.get('notes');
  Label5.caption:=msg.get('author');
  Label8.caption:=msg.get('notes');
end;

procedure TFormProject.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  VerifyClose(Accepted, ErrorData, CanClose);
end;

procedure TFormProject.OKButtonClick(Sender: TObject);
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
  aProject.Name := TeditName.Text;
  aProject.notes := TMemoNotes.Text;
  aProject.createdBy := TeditCreatedBy.TextHint;
  Accepted := True;
end;

procedure TFormProject.CancelButtonClick(Sender: TObject);
begin
  Accepted := False;
end;

function TFormProject.Exec(argProj: TProject; soloRead: boolean = False): boolean;
begin
  aProject := argProj;

  TeditName.Text := aProject.Name;
  TeditCreatedBy.Text := aProject.createdBy;
  TMemoNotes.Text := aProject.notes;

  ButtonPanel1.OKButton.Enabled := not soloRead;

  Self.ShowModal;
  Result := Accepted;
end;

function TFormProject.ExecNew(argProj: TProject): boolean;

var
  projNameIndex: string;
begin
  projNameIndex := '1';
  argProj.Name := msg.Get('ProjectDefaultName') + projNameIndex;
  Result := Exec(argProj);
end;

end.
