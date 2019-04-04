unit guiFormProject;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, ButtonPanel, Buttons, ComCtrls, Spin, Menus, MisUtils,
  BasicGrilla, sketchDocument, glob;

type
  { TfrmProject }
  TfrmProject = class(TForm)
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
    function Exec(argProj: TProject; argProcRefresh: TEvRefresh;
      soloRead: boolean = False): boolean;
    function ExecNew(argProj: TProject): boolean;
  end;

var
  frmProject: TfrmProject;

implementation

{$R *.lfm}
{ TfrmProject }
procedure TfrmProject.FormCreate(Sender: TObject);
begin
  Accepted := False;
end;

procedure TfrmProject.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  VerifyClose(Accepted, ErrorData, CanClose);
end;

procedure TfrmProject.OKButtonClick(Sender: TObject);
begin
  //Validate
  if trim(TeditName.Text) = '' then
  begin
    MsgExc('Error en Cliente: ' + TeditName.Text);
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

procedure TfrmProject.CancelButtonClick(Sender: TObject);
begin
  Accepted := False;
end;

function TfrmProject.Exec(argProj: TProject; argProcRefresh: TEvRefresh;
  soloRead: boolean): boolean;
begin
  aProject := argProj;
  procRefresh := argProcRefresh;

  TeditName.Text := aProject.Name;
  TeditCreatedBy.Text := aProject.createdBy;
  TMemoNotes.Text := aProject.notes;

  ButtonPanel1.OKButton.Enabled := not soloRead;

  Self.ShowModal;
  Result := Accepted;
end;

function TfrmProject.ExecNew(argProj: TProject): boolean;

var
  projNameIndex: string;
begin
  projNameIndex := '1';
  argProj.Name := 'Proyecto' + projNameIndex;
  Result := Exec(argProj, nil);
end;

end.
